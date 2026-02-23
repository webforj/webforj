package com.webforj.kotlin.dsl.component.dialog

import com.webforj.component.dialog.Dialog
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Dialog` with optional [header] and [footer] sections.
 * ```
 * dialog {
 *   header {
 *     label("User Registration")
 *     paragraph("Please fill in the form below")
 *   }
 *
 *   div {
 *     textField("Name", placeholder = "Enter your full name")
 *     textField("Email", placeholder = "Enter your email address", type = TextField.Type.EMAIL)
 *     passwordField("Password", placeholder = "Enter a secure password")
 *   }
 *
 *   div {
 *     checkBox("I agree to the terms and conditions")
 *     checkBox("Send me promotional emails", checked = false)
 *   }
 *
 *   footer {
 *     div {
 *       button("Cancel", theme = ButtonTheme.DANGER)
 *       button("Register", theme = ButtonTheme.PRIMARY)
 *     }
 *   }
 * }
 * ```
 *
 * To configure the sections of the `Dialog` see:
 * - [header],
 * - [footer]
 *
 * @param block The initialization steps of the `Dialog`.
 * @return The configured `Dialog`.
 * @see Dialog
 */
fun @WebforjDsl HasComponents.dialog(block: @WebforjDsl Dialog.() -> Unit = {}): Dialog {
  val dialog = Dialog()
  return init(dialog, block)
}

/**
 * Configures the components to add to the header section of a `Dialog`.
 * ```
 * dialog {
 *   header {
 *     label("Dialog Title")
 *     paragraph("Dialog description")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the header components.
 */
fun @WebforjDsl Dialog.header(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Dialog::addToHeader)
}

/**
 * Configures the components to add to the footer section of a `Dialog`.
 * ```
 * dialog {
 *   footer {
 *     div {
 *       button("Cancel")
 *       button("Save", theme = ButtonTheme.PRIMARY)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footer components.
 */
fun @WebforjDsl Dialog.footer(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Dialog::addToFooter)
}

