package com.webforj.kotlin.dsl.component.tabbedpane

import com.webforj.component.Component
import com.webforj.component.tabbedpane.Tab
import com.webforj.component.tabbedpane.TabbedPane
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `TabbedPane` with an optional [name].
 * ```
 * ... {
 *  tabbedPane() // Default TabbedPane component
 *  tabbedPane("name") // TabbedPane with name
 *  tabbedPane {
 *    tab("Dashboard")
 *    tab("Orders")
 *    tab("Customers")
 *    tab("Products")
 *    tab("Documents")
 *  }
 * }
 * ```
 *
 * @param name The name of the `TabbedPane`.
 * @param block The initialization steps of the `TabbedPane`.
 * @return The configured `TabbedPane`.
 * @see TabbedPane
 * @see tab
 */
@WebforjDsl
fun @WebforjDsl HasComponents.tabbedPane(name: String? = null, block: @WebforjDsl TabbedPane.() -> Unit = {}): TabbedPane {
  val tabbedPane = name?.let { TabbedPane(it) } ?: TabbedPane()
  return init(tabbedPane, block)
}

/**
 * The backing [Map] to allow the contentSlot of a [Tab] to be configured.
 * @see contentSlot
 */
private val tabContentMap: MutableMap<Tab, Component?> = hashMapOf()

/**
 * Creates a `Tab` with a [text] to be added to a [TabbedPane].
 * ```
 * tabbedPane {
 *  tab("text") // Adds a Tab to the TabbedPane
 * }
 * ```
 *
 * @param text The text for the `Tab`.
 * @param block The initialization steps for the `Tab`.
 * @receiver The [TabbedPane] to which the `Tab` is added.
 * @receiver The configured `Tab`.
 * @see Tab
 * @see tabbedPane
 * @see contentSlot
 * @see prefixSlot
 * @see suffixSlot
 */
@WebforjDsl
fun @WebforjDsl TabbedPane.tab(text: String, block: @WebforjDsl Tab.() -> Unit = {}): Tab {
  val tab = Tab(text)
  tab.block()
  val content = tabContentMap[tab]
  if (content != null) {
    addTab(tab, content)
  } else {
    addTab(tab)
  }
  return tab;
}

/**
 * Sets a [Component] as the contentSlot of a [Tab].
 * ```
 * tab {
 *  contentSlot {
 *    div {
 *      // tab contentSlot configuration
 *    }
 *  }
 * }
 * ```
 *
 * @param block The initialization steps of the contentSlot [Component].
 * @see tab
 * @see prefixSlot
 * @see suffixSlot
 */
@WebforjDsl
fun @WebforjDsl Tab.contentSlot(block: @WebforjDsl HasComponents.() -> Component) {
  tabContentMap[this] = SingleSlotSetter(block).component
}

/**
 * Sets a [Component] as the prefixSlot of a [Tab].
 * ```
 * tab {
 *  prefixSlot {
 *    // tab prefixSlot configuration
 *  }
 * }
 * ```
 *
 * @param block The initialization steps of the prefixSlot [Component].
 * @see tab
 * @see contentSlot
 * @see suffixSlot
 */
@WebforjDsl
fun @WebforjDsl Tab.prefixSlot(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Tab::setPrefixComponent)
}

/**
 * Sets a [Component] as the suffixSlot of a [Tab].
 * ```
 * tab {
 *  suffixSlot {
 *    // tab suffixSlot configuration
 *  }
 * }
 * ```
 *
 * @param block The initialization steps of the suffixSlot [Component].
 * @see tab
 * @see contentSlot
 * @see prefixSlot
 */
@WebforjDsl
fun @WebforjDsl Tab.suffixSlot(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Tab::setSuffixComponent)
}
