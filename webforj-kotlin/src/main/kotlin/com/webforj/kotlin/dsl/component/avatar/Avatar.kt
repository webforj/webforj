package com.webforj.kotlin.dsl.component.avatar

import com.webforj.component.avatar.Avatar
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Avatar` with an optional [label] and/or [initials].
 * ```
 * ... {
 *   avatar() // Empty Avatar component
 *   avatar("Aang", "AA") // Avatar with label and initials
 *   avatar("Bob", "B") { // Avatar with block
 *    shape = AvatarShape.SQUARE
 *   }
 * }
 * ```
 *
 * @param label The label of the `Avatar`.
 * @param initials The initials of displayed by the `Avatar`.
 * @param block The initialization steps of the `Avatar`.
 * @return The configured `Avatar`.
 * @see Avatar
 */
fun @WebforjDsl HasComponents.avatar(
  label: String? = null,
  initials: String? = null,
  block: @WebforjDsl Avatar.() -> Unit = {}
): Avatar {
  val avatar = when {
    initials != null && label != null -> Avatar(label, initials)
    label != null -> Avatar(label)
    else -> Avatar().apply {
      initials?.let { setInitials(it) }
    }
  }
  return init(avatar, block)
}