package com.webforj.kotlin.dsl.component.avatar

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class AvatarTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateEmptyAvatar() {
    val avatar = root.avatar()
    assertTrue { root.hasComponent(avatar) }
    assertEquals("Avatar", avatar.label)
    assertEquals("A", avatar.initials)
  }

  @Test
  fun shouldCreateAvatarWithLabel() {
    val avatar = root.avatar("Label")
    assertTrue { root.hasComponent(avatar) }
    assertEquals("Label", avatar.label)
    assertEquals("L", avatar.initials)
  }

  @Test
  fun shouldCreateAvatarWithInitials() {
    val avatar = root.avatar(initials = "Av")
    assertTrue { root.hasComponent(avatar) }
    assertEquals("Avatar", avatar.label)
    assertEquals("Av", avatar.initials)
  }

  @Test
  fun shouldCreateAvatarWithLabelAndInitials() {
    val avatar = root.avatar("Label", "La")
    assertTrue { root.hasComponent(avatar) }
    assertEquals("Label", avatar.label)
    assertEquals("La", avatar.initials)
  }

  @Test
  fun shouldCreateAvatarWithLabelInitialsAndBlock() {
    val avatar = root.avatar("Label", "La") {
      name = "Avatar"
    }
    assertTrue { root.hasComponent(avatar) }
    assertEquals("Label", avatar.label)
    assertEquals("La", avatar.initials)
    assertEquals("Avatar", avatar.name)
  }

}