package com.webforj.kotlin.dsl

import com.webforj.component.Component
import com.webforj.concern.HasComponents

class SingleSlotSetter(block: HasComponents.() -> Component): HasComponents {
  val component: Component = block()

  fun <T> setSlot(instance: T, setter: T.(Component) -> Unit) {
    instance.setter(component)
  }

  override fun add(vararg components: Component?) {
    /*
     * Intentionally empty - SingleSlotSetter wraps a single component
     * created by the lambda and does not support adding multiple components.
     * This maintains the HasComponents interface contract while preventing
     * unintended modification of the wrapped component.
     */
  }

  override fun remove(vararg components: Component?) {
    /*
     * Intentionally empty - SingleSlotSetter does not manage a removable
     * collection of components. The single component is managed through
     * the constructor lambda and accessed via getter methods.
     */
  }

  override fun removeAll() {
    /*
     * Intentionally empty - SingleSlotSetter cannot remove its single
     * component as it's the core purpose of this wrapper class.
     * The component lifecycle is managed by the creator, not this setter.
     */
  }

  override fun getComponentCount() = 1

  override fun getComponents() = listOf(component)

  override fun getComponent(id: String?) = component.takeIf { it.componentId == id }

}
