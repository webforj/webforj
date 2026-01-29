package com.webforj.kotlin.dsl.component.tabbedpane

import com.webforj.component.Component
import com.webforj.component.tabbedpane.Tab
import com.webforj.component.tabbedpane.TabbedPane
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy
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
fun @WebforjDsl HasComponents.tabbedPane(name: String? = null, block: @WebforjDsl TabbedPane.() -> Unit = {}): TabbedPane {
  val tabbedPane = name?.let { TabbedPane(it) } ?: TabbedPane()
  return init(tabbedPane, block)
}

/**
 * The backing [Map] to allow the content of a [Tab] to be configured.
 * @see content
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
 * @see content
 * @see prefix
 * @see suffix
 */
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
 * Sets a [Component] as the content of a [Tab].
 * ```
 * tab {
 *  content {
 *    div {
 *      // tab content configuration
 *    }
 *  }
 * }
 * ```
 *
 * @param block The initialization steps of the content [Component].
 * @see tab
 * @see prefix
 * @see suffix
 */
fun @WebforjDsl Tab.content(block: @WebforjDsl HasComponents.() -> Unit) {
  val proxy = HasComponentsProxy(block)
  tabContentMap[this] = proxy.components.first()
}

/**
 * Sets a [Component] as the prefix of a [Tab].
 * ```
 * tab {
 *  prefix {
 *    // tab prefix configuration
 *  }
 * }
 * ```
 *
 * @param block The initialization steps of the prefix [Component].
 * @see tab
 * @see content
 * @see suffix
 */
fun @WebforjDsl Tab.prefix(block: @WebforjDsl HasComponents.() -> Unit) {
  val proxy = HasComponentsProxy(block)
  prefixComponent = proxy.components.first()
}

/**
 * Sets a [Component] as the suffix of a [Tab].
 * ```
 * tab {
 *  suffix {
 *    // tab suffix configuration
 *  }
 * }
 * ```
 *
 * @param block The initialization steps of the suffix [Component].
 * @see tab
 * @see content
 * @see prefix
 */
fun @WebforjDsl Tab.suffix(block: @WebforjDsl HasComponents.() -> Unit) {
  val proxy = HasComponentsProxy(block)
  suffixComponent = proxy.components.first()
}
