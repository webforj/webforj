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
 *     beforeHeaderSlot { label("Welcome Back") }
 *     afterHeaderSlot { button("Help") }
 *     beforeContentSlot { alert("Please enter your credentials") }
 *     afterContentSlot { label("New user? Sign up") }
 *     beforeFormSlot { tablerIcon("user") }
 *     afterFormSlot { button("Forgot Password") }
 *     beforeFooterSlot { label("© 2026 My Company") }
 *     afterFooterSlot { anchor("privacy-poliy-link", "Privacy Policy") }
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
 * - [beforeHeaderSlot], and [afterHeaderSlot]
 * - [beforeContentSlot], and [afterContentSlot]
 * - [beforeFormSlot], and [afterFormSlot]
 * - [beforeFooterSlot], and [afterFooterSlot]
 * - [loginI18n] for internationalization
 *
 * @param block The initialization steps of the `Login`.
 * @return The configured `Login`.
 * @see Login
 */
@WebforjDsl
fun @WebforjDsl HasComponents.login(block: @WebforjDsl Login.() -> Unit = {}): Login = init(Login(), block)

/**
 * Configures the components to add to the before header slot of a `Login` component.
 * ```
 * login {
 *   beforeHeaderSlot {
 *     label("Welcome Back")
 *     image("logo")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before header components.
 */
@WebforjDsl
fun @WebforjDsl Login.beforeHeaderSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeHeader)
}

/**
 * Configures the components to add to the after header slot of a `Login` component.
 * ```
 * login {
 *   afterHeaderSlot {
 *     button("Help")
 *     anchor("pasword-recovery-link", "Forgot Password")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after header components.
 */
@WebforjDsl
fun @WebforjDsl Login.afterHeaderSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterHeader)
}

/**
 * Configures the components to add to the before content slot of a `Login` component.
 * ```
 * login {
 *   beforeContentSlot {
 *     alert("Please enter your credentials")
 *     label("Login to your account")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before content components.
 */
@WebforjDsl
fun @WebforjDsl Login.beforeContentSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeContent)
}

/**
 * Configures the components to add to the after content slot of a `Login` component.
 * ```
 * login {
 *   afterContentSlot {
 *     label("New user? Sign up")
 *     anchor("new-account-like", "Create account")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after content components.
 */
@WebforjDsl
fun @WebforjDsl Login.afterContentSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterContent)
}

/**
 * Configures the components to add to the before form slot of a `Login` component.
 * ```
 * login {
 *   beforeFormSlot {
 *     tablerIcon("user")
 *     label("Enter your details below")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before form components.
 */
@WebforjDsl
fun @WebforjDsl Login.beforeFormSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeForm)
}

/**
 * Configures the components to add to the after form slot of a `Login` component.
 * ```
 * login {
 *   afterFormSlot {
 *     button("Forgot Password")
 *     anchor("reset-password-link", "Reset password")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after form components.
 */
@WebforjDsl
fun @WebforjDsl Login.afterFormSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToAfterForm)
}

/**
 * Configures the components to add to the before footer slot of a `Login` component.
 * ```
 * login {
 *   beforeFooterSlot {
 *     label("© 2026 My Company")
 *     anchor("terms-of-service-link", "Terms of Service")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the before footer components.
 */
@WebforjDsl
fun @WebforjDsl Login.beforeFooterSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Login::addToBeforeFooter)
}

/**
 * Configures the components to add to the after footer slot of a `Login` component.
 * ```
 * login {
 *   afterFooterSlot {
 *     anchor("privaty-polciy-link", "Privacy Policy")
 *     anchor("contact-support-link", "Contact Support")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the after footer components.
 */
@WebforjDsl
fun @WebforjDsl Login.afterFooterSlot(block: @WebforjDsl HasComponents.() -> Unit) {
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
@WebforjDsl
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
@WebforjDsl
fun @WebforjDsl LoginI18n.loginErrorI18n(block: @WebforjDsl LoginErrorI18n.() -> Unit): LoginErrorI18n = error.apply(block)
