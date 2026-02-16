package com.webforj.kotlin.dsl.component.login

import com.webforj.component.login.Login
import com.webforj.component.login.LoginErrorI18n
import com.webforj.component.login.LoginI18n
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Login` component for user authentication forms.
 * ```
 * ... {
 *   login {
 *     beforeHeader { label("Welcome Back") }
 *     afterHeader { button("Help") }
 *     beforeContent { alert("Please enter your credentials") }
 *     afterContent { label("New user? Sign up") }
 *     beforeForm { tablerIcon("user") }
 *     afterForm { button("Forgot Password") }
 *     beforeFooter { label("© 2026 My Company") }
 *     afterFooter { anchor("privacy-poliy-link", "Privacy Policy") }
 *     loginI18n {
 *       loginErrorI18n {
 *         invalidCredentials = "Invalid username or password"
 *       }
 *     }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `Login` see:
 * - [beforeHeader], and [afterHeader]
 * - [beforeContent], and [afterContent]
 * - [beforeForm], and [afterForm]
 * - [beforeFooter], and [afterFooter]
 * - [loginI18n] for internationalization
 *
 * @param block The initialization steps of the `Login`.
 * @return The configured `Login`.
 * @see Login
 */
fun @WebforjDsl HasComponents.login(block: @WebforjDsl Login.() -> Unit = {}): Login = init(Login(), block)

/**
 * Configures the components to add to the before header slot of a `Login` component.
 * ```
 * login {
 *   beforeHeader {
 *     label("Welcome Back")
 *     image("logo")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before header components.
 */
fun @WebforjDsl Login.beforeHeader(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeHeader)
}

/**
 * Configures the components to add to the after header slot of a `Login` component.
 * ```
 * login {
 *   afterHeader {
 *     button("Help")
 *     anchor("pasword-recovery-link", "Forgot Password")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after header components.
 */
fun @WebforjDsl Login.afterHeader(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterHeader)
}

/**
 * Configures the components to add to the before content slot of a `Login` component.
 * ```
 * login {
 *   beforeContent {
 *     alert("Please enter your credentials")
 *     label("Login to your account")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before content components.
 */
fun @WebforjDsl Login.beforeContent(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeContent)
}

/**
 * Configures the components to add to the after content slot of a `Login` component.
 * ```
 * login {
 *   afterContent {
 *     label("New user? Sign up")
 *     anchor("new-account-like", "Create account")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after content components.
 */
fun @WebforjDsl Login.afterContent(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterContent)
}

/**
 * Configures the components to add to the before form slot of a `Login` component.
 * ```
 * login {
 *   beforeForm {
 *     tablerIcon("user")
 *     label("Enter your details below")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before form components.
 */
fun @WebforjDsl Login.beforeForm(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeForm)
}

/**
 * Configures the components to add to the after form slot of a `Login` component.
 * ```
 * login {
 *   afterForm {
 *     button("Forgot Password")
 *     anchor("reset-password-link", "Reset password")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after form components.
 */
fun @WebforjDsl Login.afterForm(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterForm)
}

/**
 * Configures the components to add to the before footer slot of a `Login` component.
 * ```
 * login {
 *   beforeFooter {
 *     label("© 2026 My Company")
 *     anchor("terms-of-service-link", "Terms of Service")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before footer components.
 */
fun @WebforjDsl Login.beforeFooter(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeFooter)
}

/**
 * Configures the components to add to the after footer slot of a `Login` component.
 * ```
 * login {
 *   afterFooter {
 *     anchor("privaty-polciy-link", "Privacy Policy")
 *     anchor("contact-support-link", "Contact Support")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after footer components.
 */
fun @WebforjDsl Login.afterFooter(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterFooter)
}

/**
 * Configures internationalization for the `Login` component.
 * ```
 * login {
 *   loginI18n {
 *     title = "Sign In"
 *     usernameLabel = "Email Address"
 *     passwordLabel = "Password"
 *     submitButton = "Log In"
 *     loginErrorI18n {
 *       invalidCredentials = "Invalid username or password"
 *       accountLocked = "Account is locked"
 *     }
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for `LoginI18n`.
 * @return The configured `LoginI18n`.
 * @see LoginI18n
 */
fun @WebforjDsl Login.loginI18n(block: @WebforjDsl LoginI18n.() -> Unit): LoginI18n {
  val loginI18n = LoginI18n().apply(block)
  i18n = loginI18n
  return loginI18n
}

/**
 * Configures error message internationalization for the `Login` component.
 * ```
 * loginI18n {
 *   loginErrorI18n {
 *     invalidCredentials = "Invalid username or password"
 *     accountLocked = "Account is temporarily locked"
 *     networkError = "Network connection error"
 *     serverError = "Server is not responding"
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for error messages.
 * @return The configured `LoginErrorI18n`.
 * @see LoginErrorI18n
 */
fun @WebforjDsl LoginI18n.loginErrorI18n(block: @WebforjDsl LoginErrorI18n.() -> Unit): LoginErrorI18n = error.apply(block)
