/*
 * Copyright 2025 HM Revenue & Customs
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

import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.mvc.Result
import play.api.test.Helpers._
import support.SpecCommonHelper

import java.net.URLEncoder
import scala.concurrent.Future

class RouterControllerISpec extends SpecCommonHelper {

  val responseWithEnrolments: String = {
    """
      |{
      |    "startRecord": 1,
      |    "totalRecords": 2,
      |    "enrolments": [
      |        {
      |           "service": "IR-SA",
      |           "state": "Activated",
      |           "friendlyName": "My First Client's SA Enrolment",
      |           "enrolmentDate": "2018-10-05T14:48:00.000Z",
      |           "failedActivationCount": 1,
      |           "activationDate": "2018-10-13T17:36:00.000Z",
      |           "identifiers": [
      |              {
      |                 "key": "UTR",
      |                 "value": "1234567890"
      |              }
      |           ]
      |        }
      |    ]
      |}
      |""".stripMargin
  }

  val testRouterController: RouterController = app.injector.instanceOf[RouterController]

  def authResponse(provider: String = "GovernmentGateway",
                   role: String = "User",
                   confidence: Int = 50,
                   withSA: Boolean = false,
                   withNonBusinessEnrolments: Boolean = false,
                   withNonBusinessAndOrgEnrolments: Boolean = false,
                   withMoreThanSA: Boolean = false,
                   withMtdIt: Boolean = false,
                   isAgent: Boolean = false,
                   isPTEnrolment: Boolean = false,
                   affinity: String = "Organisation",
                   profile: Option[String] = None,
                   groupProfile: Option[String] = None): JsValue = {
    val enrolments = if(withNonBusinessAndOrgEnrolments) {
      """[
        |{"key":"HMRC-CGT-PD","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-MTD-IT","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-SBI-ORG","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-NI","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-PT","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"IR-CT","identifiers": [{"key" : "UTR" , "value": "2222222227"}],"state": "Activated"}
        |]""".stripMargin
    }else if(withNonBusinessEnrolments) {
      """[
        |{"key":"HMRC-CGT-PD","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-MTD-IT","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-SBI-ORG","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-NI","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"HMRC-PT","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"IR-SA","identifiers": [{"key" : "UTR" , "value": "2222222227"}],"state": "Activated"}
        |]""".stripMargin
    }else if(withMtdIt) {
      """[
        |{"key":"HMRC-MTD-IT","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"}
        |]""".stripMargin
    }else if(withSA) {
      """[
        |{"key":"IR-SA","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"}
        |]""".stripMargin
    }else if(isAgent) {
      """[
        |{"key":"HMRC-AS-AGENT","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"}
        |]""".stripMargin
    }else if(withMoreThanSA) {
      """[
        |{"key":"IR-SA","identifiers": [{"key" : "UTR" , "value": "2222222226"}],"state": "Activated"},
        |{"key":"IR-CT","identifiers": [{"key" : "UTR" , "value": "2222222227"}],"state": "Activated"}
        |]""".stripMargin
    }else if(isPTEnrolment) {
      """[
        |{"key":"HMRC-PT","identifiers": [{"key" : "NINO" , "value": "2222222226"}],"state": "Activated"}
        |]""".stripMargin
    } else "[]"

    val base = Json.parse(s"""
              {
                 "gatewayId": "7118886894696751",
                 "optionalCredentials": {
                    "providerId": "4911434741952698",
                    "providerType": "$provider"
                 },
                 "affinityGroup": "$affinity",
                 "credentialRole": "$role",
                 "groupIdentifier": "12345678",
                 "confidenceLevel": $confidence,
                 "allEnrolments" : $enrolments
               }
               """.stripMargin).as[JsObject]

    base ++ JsObject(Seq(
      profile.map(p => "profile" -> JsString(p)),
      groupProfile.map(g => "groupProfile" -> JsString(g))
    ).flatten)
  }

  val BTA = "http://localhost:9020/business-account"
  val PTA = "http://localhost:9232/personal-account"

  val profileUrl      = "http://localhost:9999/user-profile"
  val groupProfileUrl = "http://localhost:9999/group-profile"
  val redirectUri     = "/my-service/landing"

  def withRedirectUri(url: String): String = s"$url?redirect_uri=${URLEncoder.encode(redirectUri, "UTF-8")}"

  "redirectUser" must {
    "redirect the user to BTA" when {

      "when user is 50 and has MTD-IT " in {
        stubAuthorised(authResponse(withMtdIt = true, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user has all Individual enrolments and a Org enrolment" in {
        stubAuthorised(authResponse(withNonBusinessAndOrgEnrolments = true, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user has only SA enrolments and has 50 CL" in {
        stubAuthorised(authResponse(withSA = true, confidence= 50, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user has non-SA enrolments" in {
        stubAuthorised(authResponse(withMoreThanSA = true, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user has PT enrolments and has Organisation affinity" in {
        stubAuthorised(authResponse(isPTEnrolment = true).toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user has provider = Verify and has Organisation affinity" in {
        stubAuthorised(authResponse(provider = "Verify").toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user is admin and has Organisation affinity verified with enrolments" in {
        stubAuthorised(authResponse(confidence = 200, withSA = true).toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user is admin and has Organisation affinity with SA only enrolments" in {
        stubAuthorised(authResponse(withSA = true).toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }

      "when user is admin and has Organisation affinity with more than SA enrolments" in {
        stubAuthorised(authResponse(withMoreThanSA = true).toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe BTA
      }
    }

    "redirect the user to PTA" when {

      "when user is an assistant" in {
        stubAuthorised(authResponse(role = "Assistant", affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }
      "when user has only SA enrolments and has gone through IV" in {
        stubAuthorised(authResponse(withSA = true, confidence= 200, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }

      "authenticated by only Individual enrolments" in {
        stubAuthorised(authResponse(withNonBusinessEnrolments = true, confidence=200, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }
      "authenticated by Verify instead of Gateway" in {
        stubAuthorised(authResponse(provider = "Verify", affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }
      "has only SA enrolments and has gone through IV" in {
        stubAuthorised(authResponse(withSA = true, confidence = 200, affinity = "Individual").toString)
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }
      "when group has no enrolments and has Individual affinity" in {
        stubAuthorised(authResponse(affinity = "Individual").toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }

      "when user has PT enrolment" in {
        stubAuthorised(authResponse(isPTEnrolment = true, affinity = "Individual").toString())
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe PTA
      }
    }

    "redirect the user to Agents (Classic)" when {
      "when group has no enrolments and user has no active Agent enrolment" in {
        stubAuthorised(authResponse(affinity = "Agent").toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe "http://localhost:9440/agent-usher/optin"
      }
    }

    "redirect the user to Agent Services" when {
      "when group has no enrolments and user has active Agent" in {
        stubAuthorised(authResponse(isAgent = true, affinity = "Agent").toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(None)(fakeRequest)

        redirectLocation(route).get mustBe "http://localhost:9401/agent-services-account"
      }
    }

    "redirect the user to profile management" when {
      "redirect_uri is provided and user has Individual affinity" in {
        stubAuthorised(authResponse(affinity = "Individual", profile = Some(profileUrl)).toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(Some(redirectUri))(fakeRequest)

        status(route) mustBe FOUND
        redirectLocation(route).get mustBe withRedirectUri(profileUrl)
      }
    }

    "redirect the user to group profile management" when {
      "redirect_uri is provided and user has Organisation affinity" in {
        stubAuthorised(authResponse(affinity = "Organisation", groupProfile = Some(groupProfileUrl)).toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(Some(redirectUri))(fakeRequest)

        status(route) mustBe FOUND
        redirectLocation(route).get mustBe withRedirectUri(groupProfileUrl)
      }

      "redirect_uri is provided and user has Agent affinity" in {
        stubAuthorised(authResponse(affinity = "Agent", groupProfile = Some(groupProfileUrl)).toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(Some(redirectUri))(fakeRequest)

        status(route) mustBe FOUND
        redirectLocation(route).get mustBe withRedirectUri(groupProfileUrl)
      }
    }

    "redirect the user to GG login" when {
      "redirect_uri is provided but auth returns no profile for an Individual" in {
        stubAuthorised(authResponse(affinity = "Individual").toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(Some(redirectUri))(fakeRequest)

        status(route) mustBe SEE_OTHER
        redirectLocation(route).get must include("/bas-gateway/sign-in")
      }

      "redirect_uri is provided but auth returns no groupProfile for an Organisation" in {
        stubAuthorised(authResponse(affinity = "Organisation").toString)
        noEnrolments()
        val route: Future[Result] = testRouterController.redirectUser(Some(redirectUri))(fakeRequest)

        status(route) mustBe SEE_OTHER
        redirectLocation(route).get must include("/bas-gateway/sign-in")
      }
    }

  }

}