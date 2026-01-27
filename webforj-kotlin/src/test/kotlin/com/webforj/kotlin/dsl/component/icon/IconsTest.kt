package com.webforj.kotlin.dsl.component.icon

import com.webforj.component.Theme
import com.webforj.component.html.elements.Div
import com.webforj.component.icons.DwcIcon
import com.webforj.component.icons.FeatherIcon
import com.webforj.component.icons.Icon
import com.webforj.component.icons.FontAwesomeIcon
import com.webforj.component.icons.TablerIcon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.icons.dwcIcon
import com.webforj.kotlin.dsl.component.icons.featherIcon
import com.webforj.kotlin.dsl.component.icons.fontAwesomeIcon
import com.webforj.kotlin.dsl.component.icons.icon
import com.webforj.kotlin.dsl.component.icons.iconButton
import com.webforj.kotlin.dsl.component.icons.tablerIcon
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.BeforeEach
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class IconsTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateIconWithNameAndPool() {
    val icon = root.icon("icon", "pool")
    assertTrue { root.hasComponent(icon) }
    assertEquals("icon", icon.name)
    assertEquals("pool", icon.pool)
  }

  @Test
  fun shouldCreateIconWithNamePoolAndBlock() {
    val icon = root.icon("icon", "pool") {
      isVisible = false
    }
    assertTrue { root.hasComponent(icon) }
    assertEquals("icon", icon.name)
    assertEquals("pool", icon.pool)
    assertFalse { icon.isVisible }
  }

  @Test
  fun shouldCreateIconButtonWithNameAndPool() {
    val icon = root.iconButton("icon-button", "pool")
    assertTrue { root.hasComponent(icon) }
    assertEquals("icon-button", icon.name)
    assertEquals("pool", icon.pool)
  }

  @Test
  fun shouldCreateIconButtonWithNamePoolAndBlock() {
    val icon = root.iconButton("icon", "pool") {
      isVisible = false
    }
    assertTrue { root.hasComponent(icon) }
    assertEquals("icon", icon.name)
    assertEquals("pool", icon.pool)
    assertFalse { icon.isVisible }
  }

  @Test
  fun shouldCreateIconButtonWithIcon() {
    val icon = root.iconButton(Icon("icon", "pool"))
    assertTrue { root.hasComponent(icon) }
    assertEquals("icon", icon.name)
    assertEquals("pool", icon.pool)
  }

  @Test
  fun shouldCreateFontAwesomeIconWithName() {
    val icon = root.fontAwesomeIcon("user")
    assertTrue { root.hasComponent(icon) }
    assertEquals("user", icon.name)
    assertEquals("fa", icon.pool)
  }

  @Test
  fun shouldCreateFontAwesomeIconWithNameAndVariant() {
    val icon = root.fontAwesomeIcon("user", FontAwesomeIcon.Variate.SOLID)
    assertTrue { root.hasComponent(icon) }
    assertEquals("fas-user", icon.name)
    assertEquals("fa", icon.pool)
  }

  @Test
  fun shouldCreateFontAwesomeIconWithNameAndBlock() {
    val icon = root.fontAwesomeIcon("heart") {
      isVisible = false
    }
    assertTrue { root.hasComponent(icon) }
    assertEquals("heart", icon.name)
    assertEquals("fa", icon.pool)
    assertFalse { icon.isVisible }
  }

  @Test
  fun shouldCreateFontAwesomeIconWithNameVariantAndBlock() {
    val icon = root.fontAwesomeIcon("star", FontAwesomeIcon.Variate.REGULAR) {
      isVisible = true
    }
    assertTrue { root.hasComponent(icon) }
    assertEquals("far-star", icon.name)
    assertEquals("fa", icon.pool)
    assertTrue { icon.isVisible }
  }

  @Test
  fun shouldCreateTablerIconWithName() {
    val icon = root.tablerIcon("bell")
    assertTrue { root.hasComponent(icon) }
    assertEquals("bell", icon.name)
    assertEquals("tabler", icon.pool)
  }

  @Test
  fun shouldCreateTablerIconWithNameAndBlock() {
    val icon = root.tablerIcon("settings") {
      isVisible = false
    }
    assertTrue { root.hasComponent(icon) }
    assertEquals("settings", icon.name)
    assertEquals("tabler", icon.pool)
    assertFalse { icon.isVisible }
  }

  @Test
  fun shouldCreateTablerIconWithNameAndVariant() {
    val icon = root.tablerIcon("bell", TablerIcon.Variate.FILLED)
    assertTrue { root.hasComponent(icon) }
    assertEquals("filled-bell", icon.name)
    assertEquals("tabler", icon.pool)
  }

  @Test
  fun shouldCreateTablerIconWithNameVariantAndBlock() {
    val icon = root.tablerIcon("home", TablerIcon.Variate.OUTLINE) {
      isVisible = true
    }
    assertTrue { root.hasComponent(icon) }
    assertEquals("outline-home", icon.name)
    assertEquals("tabler", icon.pool)
    assertTrue { icon.isVisible }
  }

  @Test
  fun shouldCreateIconButtonWithFontAwesomeIcon() {
    val fontAwesomeIcon = FontAwesomeIcon.create("user", FontAwesomeIcon.Variate.SOLID)
    val iconButton = root.iconButton(fontAwesomeIcon)
    assertTrue { root.hasComponent(iconButton) }
    assertEquals("fas-user", iconButton.name)
    assertEquals("fa", iconButton.pool)
  }

  @Test
  fun shouldCreateIconButtonWithTablerIcon() {
    val tablerIcon = TablerIcon.create("bell", TablerIcon.Variate.FILLED)
    val iconButton = root.iconButton(tablerIcon)
    assertTrue { root.hasComponent(iconButton) }
    assertEquals("filled-bell", iconButton.name)
    assertEquals("tabler", iconButton.pool)
  }

  @Test
  fun shouldCreateDwcIcon() {
    val checkIcon = root.dwcIcon(DwcIcon.CHECK)
    assertTrue { root.hasComponent(checkIcon) }
    assertEquals(DwcIcon.CHECK.toString(), checkIcon.name)
    assertEquals(DwcIcon.CHECK.pool, checkIcon.pool)
    assertEquals(Theme.DEFAULT, checkIcon.theme)
    val plusIcon = root.dwcIcon(DwcIcon.PLUS) {
      theme = Theme.SUCCESS
    }
    assertTrue { root.hasComponent(plusIcon) }
    assertEquals(DwcIcon.PLUS.toString(), plusIcon.name)
    assertEquals(DwcIcon.PLUS.pool, plusIcon.pool)
    assertEquals(Theme.SUCCESS, plusIcon.theme)
  }

  @Test
  fun shouldCreateFeatherIcon() {
    val saveIcon = root.featherIcon(FeatherIcon.SAVE)
    assertTrue { root.hasComponent(saveIcon) }
    assertEquals(FeatherIcon.SAVE.toString(), saveIcon.name)
    assertEquals(FeatherIcon.SAVE.pool, saveIcon.pool)
    assertEquals(Theme.DEFAULT, saveIcon.theme)
    val phoneMissedIcon = root.featherIcon(FeatherIcon.PHONE_MISSED) {
      theme = Theme.DANGER
    }
    assertTrue { root.hasComponent(phoneMissedIcon) }
    assertEquals(FeatherIcon.PHONE_MISSED.toString(), phoneMissedIcon.name)
    assertEquals(FeatherIcon.PHONE_MISSED.pool, phoneMissedIcon.pool)
    assertEquals(Theme.DANGER, phoneMissedIcon.theme)
  }
}
