package com.webforj.kotlin.dsl

import com.webforj.component.Component
import com.webforj.concern.HasComponents

class SingleSlotSetter(block: HasComponents.() -> Component): HasComponents {
  val component: Component = block()

  fun <T> setSlot(instance: T, setter: T.(Component) -> Unit) {
    instance.setter(component)
  }

  override fun add(vararg components: Component?) {}

  override fun remove(vararg components: Component?) {}

  override fun removeAll() {}

  override fun getComponentCount() = 1

  override fun getComponents() = listOf(component)

  override fun getComponent(id: String?) = component.takeIf { it.componentId == id }

}
