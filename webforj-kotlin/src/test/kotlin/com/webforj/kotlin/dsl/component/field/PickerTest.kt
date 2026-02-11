package com.webforj.kotlin.dsl.component.field

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertFalse

class PickerTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldModifyDatePicker() {
    val field = root.maskedDateField {
      picker {
        isIconVisible = false
      }
    }
    assertFalse { field.picker.isIconVisible }
  }

  @Test
  fun shouldModifyTimePicker() {
    val field = root.maskedTimeField {
      picker {
        isIconVisible = false
      }
    }
    assertFalse { field.picker.isIconVisible }
  }
}
