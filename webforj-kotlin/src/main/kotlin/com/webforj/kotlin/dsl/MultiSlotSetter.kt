package com.webforj.kotlin.dsl

import com.webforj.component.Component
import com.webforj.concern.HasComponents

/**
 * A proxy for [HasComponents] to be used to add [Component] instances to the slots of other `Components`.
 *
 * Implements the methods in [HasComponents] to use a backing [MutableList].
 */
class MultiSlotSetter(block: HasComponents.() -> Unit): HasComponents {
  internal val backingList: MutableList<Component> = arrayListOf()

  init {
    block()
  }

  /**
   * Adds the `Components` to the given [component] by invoking the [setter]
   * with the backing list of [Component] instances.
   *
   * @param component The [Component] whose slot is going to be set.
   * @param setter The steps to set the `Components`.
   */
  fun <T: Component> setSlot(component: T, setter: T.(Array<Component?>) -> Unit) {
    component.setter(backingList.toTypedArray())
  }

  override fun add(vararg components: Component) {
    backingList.addAll(components)
  }

  override fun remove(vararg components: Component) {
    backingList.removeAll(components)
  }

  override fun removeAll() {
    backingList.clear()
  }

  override fun getComponentCount(): Int = backingList.size

  override fun getComponents(): List<Component?> = listOf(*backingList.toTypedArray())

  override fun getComponent(id: String?): Component? = backingList.find { id == it.componentId }

}
