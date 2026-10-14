/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers

import actions.AuthAction
import config.FrontendAppConfig
import model.UserProfile
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services._
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.ConfidenceLevel.L200
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import utils.LoggingUtil

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RouterController @Inject()(val authConnector: AuthConnector,
                                 authAction: AuthAction,
                                 appConfig: FrontendAppConfig,
                                 val messagesControllerComponents: MessagesControllerComponents,
                                 auditService: AuditService)
                                (implicit val ec: ExecutionContext)
  extends FrontendController(messagesControllerComponents) with AuthorisedFunctions with LoggingUtil {

  private val saEnrolments: Set[String]          = Set("IR-SA", "HMRC-MTD-IT", "HMRC-NI")
  private val mtdOrSaEnrolments: Set[String]     = Set("IR-SA", "HMRC-MTD-IT")
  private val nonBusinessEnrolments: Set[String] = Set("HMRC-CGT-PD", "IR-SA", "HMRC-MTD-IT", "HMRC-SBI-ORG", "HMRC-NI", "HMRC-PT")

  def redirectUser(redirect_uri: Option[String]): Action[AnyContent] = Action.async { implicit request =>
    authAction.userProfile { user =>
      val keys = user.enrolments.map(_.key)

      if (redirect_uri.isDefined && isGovernmentGateway(user)) toProfileManagement(redirect_uri, user)
      else user.affinityGroup match {
        case Some(Organisation) =>
          toBTA(organisationReason(user, keys), user)

        case Some(Agent) =>
          if (keys.contains("HMRC-AS-AGENT")) route("agent-services", "AgentServices", Redirect(appConfig.agents), user)
          else route("agent-classic", "AgentClassic", Redirect(appConfig.agentsClassic), user)

        case Some(Individual) =>
          if (keys.exists(k => !nonBusinessEnrolments(k))) toBTA("has-business-enrolment", user)
          else if (user.confidenceLevel < L200 && keys.exists(mtdOrSaEnrolments)) toBTA("verified-with-SA", user)
          else toPTA("individuals-not-verified-doesnt-have-business-enrolments", user)

        case _ =>
          toBTA("Everyone else - SHOULD NOT GET HERE", user)
      }
    }
  }

  private def isGovernmentGateway(user: UserProfile): Boolean =
    user.credentials.exists(_.providerType == "GovernmentGateway")

  private def organisationReason(user: UserProfile, keys: Set[String]): String = {
    val isAdmin          = user.credentialRole.contains(User)
    val onlySaEnrolments = keys.nonEmpty && keys.subsetOf(saEnrolments)
    val nonSaEnrolments  = (keys - "HMRC-AS-AGENT").exists(k => !saEnrolments(k))

    if (keys.contains("HMRC-PT"))
      "user-having-OrganisationAffinity-and-PTEnrolment-routed-to-BTA-would-have-gone-to-PTA"
    else if (user.credentials.exists(_.providerType == "Verify"))
      "user-having-OrganisationAffinity-and-is-not-a-gatewayUser-routed-to-BTA-would-have-gone-to-PTA"
    else if (isAdmin && onlySaEnrolments && user.confidenceLevel >= L200)
      "user-with-OrganisationAffinity-Admin-sa-enrolments-and-cl250-routed-to-BTA-would-have-gone-to-PTA"
    else if (isAdmin && onlySaEnrolments)
      "user-with-OrganisationAffinity-Admin-sa-enrolments-and-not-cl250-routed-to-BTA-would-have-gone-to-BTA"
    else if (isAdmin && nonSaEnrolments)
      "user-with-OrganisationAffinity-no-sa-enrolments-routed-to-BTA-would-have-gone-to-BTA"
    else
      "user-with-orgAffinity-routed-to-BTA-would-have-gone-to-BTA"
  }

  private def toProfileManagement(redirect_uri: Option[String], user: UserProfile)
                                 (implicit hc: HeaderCarrier, request: Request[_]): Future[Result] = {
    val isGroup = user.affinityGroup.exists(ag => ag == Organisation || ag == Agent)

    val (retrieval, location, reason, actionName) =
      if (isGroup) (Retrievals.groupProfile, "GroupProfileManagement", "group-profile-management", "redirectToGroupProfileManagement")
      else         (Retrievals.profile, "ProfileManagement", "profile-management", "redirectToProfileManagement")

    authorised().retrieve(retrieval) {
      case Some(profileUrl) =>
        route(reason, location, Redirect(profileUrl, Map("redirect_uri" -> redirect_uri.toSeq), FOUND), user)
      case None =>
        Future.successful(toGGLogin(request.uri))
    } recover {
      case _: NoActiveSession => toGGLogin(request.uri)
      case ex =>
        logger.error(s"Exception performing $actionName", ex)
        throw ex
    }
  }

  private def toPTA(reason: String, user: UserProfile)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    route(reason, "PTA", Redirect(appConfig.pta), user)

  private def toBTA(reason: String, user: UserProfile)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    route(reason, "BTA", Redirect(appConfig.bta), user)

  private def route(reason: String, location: String, destination: Result, user: UserProfile)
                   (implicit hc: HeaderCarrier, request: Request[_]): Future[Result] = {
    auditService.audit(AuditModel(reason, auditData(reason, location, user)))
    infoLog(s"[RouterController][route] $location $reason")
    Future.successful(destination)
  }

  private def auditData(reason: String, location: String, user: UserProfile): JsObject =
    Json.obj(
      "reason"          -> reason,
      "location"        -> location,
      "enrolments"      -> Json.toJson(user.enrolments),
      "affinityGroup"   -> user.affinityGroup.fold("")(_.toString),
      "confidenceLevel" -> user.confidenceLevel.level.toString,
      "credentials"     -> user.credentials.fold("")(_.toString),
      "credentialRole"  -> user.credentialRole.fold("")(_.toString)
    )

  private def toGGLogin(continueUrl: String): Result =
    Redirect(appConfig.ggLoginUrl, Map("continue_url" -> Seq(continueUrl), "origin" -> Seq(appConfig.origin)))
}