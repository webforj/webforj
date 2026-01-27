package com.webforj.kotlin.dsl.component.dialog

import com.webforj.component.button.ButtonTheme
import com.webforj.component.field.TextField
import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.field.passwordField
import com.webforj.kotlin.dsl.component.field.textField
import com.webforj.kotlin.dsl.component.html.elements.div
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import com.webforj.kotlin.dsl.component.optioninput.checkBox
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class DialogTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateDialog() {
    val dialog = root.dialog()
    assertTrue { root.hasComponent(dialog) }
  }

  @Test
  fun shouldCreateHeader() {
    val dialog = root.dialog {
      header {
        label("Dialog Title")
        paragraph("Dialog description")
      }
    }
    
    assertNotNull(dialog)
    assertTrue { root.hasComponent(dialog) }
    assertEquals(2, dialog.componentCount)
  }

  @Test
  fun shouldCreateFooter() {
    val dialog = root.dialog {
      footer {
        div {
          button("Cancel")
          button("Save", theme = ButtonTheme.PRIMARY)
        }
      }
    }
    
    assertNotNull(dialog)
    assertTrue { root.hasComponent(dialog) }
    assertEquals(1, dialog.componentCount)
  }

  @Test
  fun shouldCreateExample() {
    val dialog = root.dialog {
      header {
        div {
          label("User Registration")
          paragraph("Please fill in the form below to create your account")
        }
      }
      
      div {
          textField("Name", placeholder = "Enter your full name")
          textField("Email", placeholder = "Enter your email address", type = TextField.Type.EMAIL)
          passwordField("Password", placeholder = "Enter a secure password")
      }

      div {
        checkBox("I agree to the terms and conditions")
        checkBox("Send me promotional emails", checked = false)
      }
      
      footer {
        div {
          button("Cancel", theme = ButtonTheme.DANGER)
          button("Register", theme = ButtonTheme.PRIMARY)
        }
      }
    }
    
    assertNotNull(dialog)
    assertTrue { root.hasComponent(dialog) }
    assertEquals(4, dialog.componentCount)
  }

}
