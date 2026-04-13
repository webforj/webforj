package com.webforj.kotlin.dsl.component.terminal

import com.webforj.component.terminal.Terminal
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Terminal`.
 * ```
 * ... {
 *   terminal {
 *       isAutoFit = true
 *       size = 95.percent to 95.percent
 *       styles["margin"] = "var(--dwc-space-m)"
 *       writeln("\u001B[1;32mWelcome 👋  to the webforJ terminal!\u001B[0m")
 *       writeln("Type \u001B[1;33m`help`\u001B[0m to see a list of supported commands.")
 *       writeln("$ ")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the `Terminal`.
 * @return The configured `Terminal`.
 *
 * @see Terminal
 */
fun @WebforjDsl HasComponents.terminal(block: @WebforjDsl Terminal.() -> Unit = {}) = init(Terminal(), block)
