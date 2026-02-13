package com.webforj.kotlin.extension

import com.webforj.concern.HasSize

/**
 * Provides a size property for components that implement [HasSize].
 * 
 * This property allows getting and setting both width and height as a pair of strings.
 * The first element represents width and the second represents height.
 * 
 * Example:
 * ```
 * component.size = "100px" to "200px"
 * val (width, height) = component.size
 * ```
 */
var HasSize<*>.size: Pair<String, String>
  get() = width to height
  set(value) { setSize(value.first, value.second) }

/**
 * Provides a minSize property for components that implement [HasSize].
 * 
 * This property allows getting and setting both minimum width and height as a pair of strings.
 * The first element represents minimum width and the second represents minimum height.
 * 
 * Example:
 * ```
 * component.minSize = "50px" to "100px"
 * val (minWidth, minHeight) = component.minSize
 * ```
 */
var HasSize<*>.minSize: Pair<String, String>
  get() = minWidth to minHeight
  set(value) { setMinSize(value.first, value.second) }

/**
 * Provides a maxSize property for components that implement [HasSize].
 * 
 * This property allows getting and setting both maximum width and height as a pair of strings.
 * The first element represents maximum width and the second represents maximum height.
 * 
 * Example:
 * ```
 * component.maxSize = "500px" to "800px"
 * val (maxWidth, maxHeight) = component.maxSize
 * ```
 */
var HasSize<*>.maxSize: Pair<String, String>
  get() = maxWidth to maxHeight
  set(value) { setMaxSize(value.first, value.second) }
