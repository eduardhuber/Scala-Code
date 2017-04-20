package controllers

import play.api.libs.json.Json

object Errors {
  // When user tries to change their email or signup with an email, but email
  // entered is already taken.
  val emailExistsError = Json.obj("error" -> "email has already been taken")

  // Email not Found
  val emailNotFoundError = Json.obj("error" -> "Email Not Found")

  // Each user is allowed three messages per 24 hours.
  val quotaExceededError = Json.obj("error" -> "quota_exceeded")

  // When inviting a user by email, and either:
  // - that user already has an invitation, and a different email is used, or
  // - another user was invited with that email.
  val emailMismatchError = Json.obj("error" -> "email_mismatch")

  // When the user has connection requests disabled in settings.
  val disabledMemberConnectionsError = Json.obj("error" -> "connections_disabled")
}
