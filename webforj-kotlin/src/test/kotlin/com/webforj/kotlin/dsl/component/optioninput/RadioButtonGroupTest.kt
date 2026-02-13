package com.webforj.kotlin.dsl.component.optioninput

import com.webforj.component.html.elements.Div
import com.webforj.component.optioninput.RadioButton
import com.webforj.component.optioninput.RadioButtonGroup
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class RadioButtonGroupTest {

  companion object Providers {

    @Suppress("UNCHECKED_CAST")
    @JvmStatic
    fun provideHasComponentsRadioButton(): List<Array<Any>> {
      val expectedText = "text"
      val expectedName = "name"
      val list = arrayListOf<Array<Any>>()
      for (text in listOf(null, expectedText)) {
        for (checked in listOf(null, false, true)) {
          for (name in listOf(null, expectedName)) {
            for (activation in RadioButton.Activation.entries) {
              val root = Div()
              list.add(
                arrayOf(text, checked, name, activation, root, root.radioButton(text, checked, name) {
                  this.activation = activation
                }) as Array<Any>
              )
            }
          }
        }
      }
      return list
    }

    @Suppress("UNCHECKED_CAST")
    @JvmStatic
    fun provideHasComponentsSwitch(): List<Array<Any>> {
      val expectedText = "text"
      val expectedName = "name"
      val list = arrayListOf<Array<Any>>()
      for (text in listOf(null, expectedText)) {
        for (checked in listOf(null, false, true)) {
          for (name in listOf(null, expectedName)) {
            for (activation in RadioButton.Activation.entries) {
              val root = Div()
              list.add(
                arrayOf(text, checked, name, activation, root, root.switch(text, checked, name) {
                  this.activation = activation
                }) as Array<Any>
              )
            }
          }
        }
      }
      return list
    }

    @Suppress("UNCHECKED_CAST")
    @JvmStatic
    fun provideGroupRadioButton(): List<Array<Any>> {
      val expectedText = "text"
      val expectedName = "name"
      val list = arrayListOf<Array<Any>>()
      for (text in listOf(null, expectedText)) {
        for (checked in listOf(null, false, true)) {
          for (name in listOf(null, expectedName)) {
            for (activation in RadioButton.Activation.entries) {
              val root = RadioButtonGroup()
              list.add(
                arrayOf(text, checked, name, activation, root, root.radioButton(text, checked, name) {
                  this.activation = activation
                }) as Array<Any>
              )
            }
          }
        }
      }
      return list
    }

    @Suppress("UNCHECKED_CAST")
    @JvmStatic
    fun provideGroupSwitch(): List<Array<Any>> {
      val expectedText = "text"
      val expectedName = "name"
      val list = arrayListOf<Array<Any>>()
      for (text in listOf(null, expectedText)) {
        for (checked in listOf(null, false, true)) {
          for (name in listOf(null, expectedName)) {
            for (activation in RadioButton.Activation.entries) {
              val root = RadioButtonGroup()
              list.add(
                arrayOf(text, checked, name, activation, root, root.switch(text, checked, name) {
                  this.activation = activation
                }) as Array<Any>
              )
            }
          }
        }
      }
      return list
    }

  }

  @Test
  fun shouldCreateEmptyRadioButtonGroup() {
    val root = Div()
    val group = root.radioButton()
    assertTrue { root.hasComponent(group) }
    assertEquals("", group.text)
  }

  @Test
  fun shouldCreateRadioButtonGroupWithText() {
    val text = "RadioButtonGroup"
    val root = Div()
    val group = root.radioButton(text)
    assertTrue { root.hasComponent(group) }
    assertEquals(text, group.text)
  }

  @Test
  fun shouldCreateRadioButtonGroupWithBlock() {
    val text = "RadioButtonGroup"
    val root = Div()
    val group = root.radioButton { name = text }
    assertTrue { root.hasComponent(group) }
    assertEquals("", group.text)
    assertEquals(text, group.name)
  }

  @Test
  fun shouldCreateRadioButtonGroupWithTextAndBlock() {
    val text = "RadioButtonGroup"
    val expected = "name"
    val root = Div()
    val group = root.radioButton(text) { name = expected }
    assertTrue { root.hasComponent(group) }
    assertEquals(text, group.text)
    assertEquals(expected, group.name)
  }

  @ParameterizedTest(name = "Create RadioButton with text={0}, checked={1}, name={2} and activation={3}.")
  @MethodSource("provideHasComponentsRadioButton")
  fun shouldCreateRadioButtonByHasComponents(
    text: String?,
    checked: Boolean?,
    name: String?,
    activation: RadioButton.Activation,
    root: HasComponents,
    radioButton: RadioButton
  ) {
    assertTrue { root.hasComponent(radioButton) }
    assertEquals(text ?: "", radioButton.text)
    assertEquals(checked ?: false, radioButton.isChecked)
    assertEquals(name ?: text ?: "", radioButton.name)
    assertEquals(activation, radioButton.activation)
  }

  @ParameterizedTest(name = "Create Switch with text={0}, checked={1}, name={2} and activation={3}.")
  @MethodSource("provideHasComponentsSwitch")
  fun shouldCreateSwitchByHasComponents(
    text: String?,
    checked: Boolean?,
    name: String?,
    activation: RadioButton.Activation,
    root: HasComponents,
    radioButton: RadioButton
  ) {
    assertTrue { root.hasComponent(radioButton) }
    assertEquals(text ?: "", radioButton.text)
    assertEquals(checked ?: false, radioButton.isChecked)
    assertEquals(name ?: text ?: "", radioButton.name)
    assertEquals(activation, radioButton.activation)
  }

  @ParameterizedTest(name = "Create RadioButton with text={0}, checked={1}, name={2} and activation={3}.")
  @MethodSource("provideGroupRadioButton")
  fun shouldCreateRadioButtonByRadioButtonGroup(
    text: String?,
    checked: Boolean?,
    name: String?,
    activation: RadioButton.Activation,
    root: RadioButtonGroup,
    radioButton: RadioButton
  ) {
    assertTrue { root.buttons.contains(radioButton) }
    assertEquals(text ?: "", radioButton.text)
    assertEquals(checked ?: false, radioButton.isChecked)
    assertEquals(name ?: text ?: "", radioButton.name)
    assertEquals(activation, radioButton.activation)
  }

  @ParameterizedTest(name = "Create Switch with text={0}, checked={1}, name={2} and activation={3}.")
  @MethodSource("provideGroupSwitch")
  fun shouldCreateSwitchByRadioButtonGroup(
    text: String?,
    checked: Boolean?,
    name: String?,
    activation: RadioButton.Activation,
    root: RadioButtonGroup,
    radioButton: RadioButton
  ) {
    assertTrue { root.buttons.contains(radioButton) }
    assertEquals(text ?: "", radioButton.text)
    assertEquals(checked ?: false, radioButton.isChecked)
    assertEquals(name ?: text ?: "", radioButton.name)
    assertEquals(activation, radioButton.activation)
  }

}
