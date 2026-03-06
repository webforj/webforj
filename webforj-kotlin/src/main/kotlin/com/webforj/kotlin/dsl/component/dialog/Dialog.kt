package com.webforj.kotlin.dsl.component.dialog

import com.webforj.component.dialog.Dialog
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Dialog` with optional [headerSlot] and [footerSlot] sections.
 * ```
 * dialog {
 *   headerSlot {
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
 *   footerSlot {
 *     div {
 *       button("Cancel", theme = ButtonTheme.DANGER)
 *       button("Register", theme = ButtonTheme.PRIMARY)
 *     }
 *   }
 * }
 * ```
 *
 * To configure the sections of the `Dialog` see:
 * - [headerSlot],
 * - [footerSlot]
 *
 * @param block The initialization steps of the `Dialog`.
 * @return The configured `Dialog`.
 * @see Dialog
 */
@WebforjDsl
fun @WebforjDsl HasComponents.dialog(block: @WebforjDsl Dialog.() -> Unit = {}): Dialog {
  val dialog = Dialog()
  return init(dialog, block)
}

/**
 * Configures the components to add to the headerSlot section of a `Dialog`.
 * ```
 * dialog {
 *   headerSlot {
 *     label("Dialog Title")
 *     paragraph("Dialog description")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the headerSlot components.
 */
@WebforjDsl
fun @WebforjDsl Dialog.headerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Dialog::addToHeader)
}

/**
 * Configures the components to add to the footerSlot section of a `Dialog`.
 * ```
 * dialog {
 *   footerSlot {
 *     div {
 *       button("Cancel")
 *       button("Save", theme = ButtonTheme.PRIMARY)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footerSlot components.
 */
@WebforjDsl
fun @WebforjDsl Dialog.footerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Dialog::addToFooter)
}

