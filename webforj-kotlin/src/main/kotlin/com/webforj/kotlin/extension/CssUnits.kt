package com.webforj.kotlin.extension

/**
 * Provides extension properties for [Number] to convert values to CSS units.
 * 
 * These extensions allow for a more type-safe and readable way to create CSS values
 * with appropriate units directly from numeric values.
 */

// Absolute

/**
 * Converts the number to CSS pixels (px).
 * 
 * Pixels are absolute units commonly used for screen displays.
 * 
 * Example:
 * ```
 * val width = 100.px  // "100px"
 * ```
 */
val Number.px: String
  get() = "${this}px"

/**
 * Converts the number to CSS centimeters (cm).
 * 
 * Centimeters are absolute units based on physical measurements.
 * 1cm = 96px/2.54.
 * 
 * Example:
 * ```
 * val height = 5.cm  // "5cm"
 * ```
 */
val Number.cm: String
  get() = "${this}cm"

/**
 * Converts the number to CSS millimeters (mm).
 * 
 * Millimeters are absolute units based on physical measurements.
 * 1mm = 1/10 of 1cm.
 * 
 * Example:
 * ```
 * val margin = 10.mm  // "10mm"
 * ```
 */
val Number.mm: String
  get() = "${this}mm"

/**
 * Converts the number to CSS inches (in).
 * 
 * Inches are absolute units based on physical measurements.
 * 1in = 2.54cm = 96px.
 * 
 * Example:
 * ```
 * val width = 2.inches  // "2in"
 * ```
 */
val Number.inches: String
  get() = "${this}in"

/**
 * Converts the number to CSS points (pt).
 * 
 * Points are absolute units traditionally used in print media.
 * 1pt = 1/72 of 1in.
 * 
 * Example:
 * ```
 * val fontSize = 12.pt  // "12pt"
 * ```
 */
val Number.pt: String
  get() = "${this}pt"

/**
 * Converts the number to CSS picas (pc).
 * 
 * Picas are absolute units traditionally used in print media.
 * 1pc = 12pt = 1/6 of 1in.
 * 
 * Example:
 * ```
 * val width = 3.pc  // "3pc"
 * ```
 */
val Number.pc: String
  get() = "${this}pc"

// Relative

/**
 * Converts the number to CSS em units.
 * 
 * Em units are relative to the font size of the parent element.
 * 1em = the font size of the parent element.
 * 
 * Example:
 * ```
 * val padding = 1.5.em  // "1.5em"
 * ```
 */
val Number.em: String
  get() = "${this}em"

/**
 * Converts the number to CSS rem units.
 * 
 * Rem units are relative to the font size of the root element (html).
 * 1rem = the font size of the root element.
 * 
 * Example:
 * ```
 * val fontSize = 1.2.rem  // "1.2rem"
 * ```
 */
val Number.rem: String
  get() = "${this}rem"

/**
 * Converts the number to CSS ex units.
 * 
 * Ex units are relative to the x-height of the current font.
 * 1ex = the height of the lowercase "x" character.
 * 
 * Example:
 * ```
 * val lineHeight = 1.5.ex  // "1.5ex"
 * ```
 */
val Number.ex: String
  get() = "${this}ex"

/**
 * Converts the number to CSS ch units.
 * 
 * Ch units are relative to the width of the "0" (zero) character of the current font.
 * 
 * Example:
 * ```
 * val width = 20.ch  // "20ch"
 * ```
 */
val Number.ch: String
  get() = "${this}ch"

// Percent

/**
 * Converts the number to CSS percentage (%).
 * 
 * Percentage units are relative to the parent element's corresponding property value.
 * 
 * Example:
 * ```
 * val width = 50.percent  // "50%"
 * ```
 */
val Number.percent: String
  get() = "${this}%"

// Viewport relative

/**
 * Converts the number to CSS viewport width (vw) units.
 * 
 * 1vw = 1% of the viewport's width.
 * 
 * Example:
 * ```
 * val width = 100.vw  // "100vw" (full viewport width)
 * ```
 */
val Number.vw: String
  get() = "${this}vw"

/**
 * Converts the number to CSS viewport height (vh) units.
 * 
 * 1vh = 1% of the viewport's height.
 * 
 * Example:
 * ```
 * val height = 100.vh  // "100vh" (full viewport height)
 * ```
 */
val Number.vh: String
  get() = "${this}vh"

/**
 * Converts the number to CSS viewport minimum (vmin) units.
 * 
 * 1vmin = 1% of the smaller of the viewport's width or height.
 * 
 * Example:
 * ```
 * val size = 50.vmin  // "50vmin"
 * ```
 */
val Number.vmin: String
  get() = "${this}vmin"

/**
 * Converts the number to CSS viewport maximum (vmax) units.
 * 
 * 1vmax = 1% of the larger of the viewport's width or height.
 * 
 * Example:
 * ```
 * val size = 50.vmax  // "50vmax"
 * ```
 */
val Number.vmax: String
  get() = "${this}vmax"

/**
 * Converts the number to CSS small viewport width (svw) units.
 * 
 * 1svw = 1% of the small viewport's width (ignoring browser UI).
 * 
 * Example:
 * ```
 * val width = 100.svw  // "100svw"
 * ```
 */
val Number.svw: String
  get() = "${this}svw"

/**
 * Converts the number to CSS small viewport height (svh) units.
 * 
 * 1svh = 1% of the small viewport's height (ignoring browser UI).
 * 
 * Example:
 * ```
 * val height = 100.svh  // "100svh"
 * ```
 */
val Number.svh: String
  get() = "${this}svh"

/**
 * Converts the number to CSS large viewport width (lvw) units.
 * 
 * 1lvw = 1% of the large viewport's width (including browser UI when not hidden).
 * 
 * Example:
 * ```
 * val width = 100.lvw  // "100lvw"
 * ```
 */
val Number.lvw: String
  get() = "${this}lvw"

/**
 * Converts the number to CSS large viewport height (lvh) units.
 * 
 * 1lvh = 1% of the large viewport's height (including browser UI when not hidden).
 * 
 * Example:
 * ```
 * val height = 100.lvh  // "100lvh"
 * ```
 */
val Number.lvh: String
  get() = "${this}lvh"

/**
 * Converts the number to CSS dynamic viewport width (dvw) units.
 * 
 * 1dvw = 1% of the dynamic viewport's width (changes when browser UI is hidden/shown).
 * 
 * Example:
 * ```
 * val width = 100.dvw  // "100dvw"
 * ```
 */
val Number.dvw: String
  get() = "${this}dvw"

/**
 * Converts the number to CSS dynamic viewport height (dvh) units.
 * 
 * 1dvh = 1% of the dynamic viewport's height (changes when browser UI is hidden/shown).
 * 
 * Example:
 * ```
 * val height = 100.dvh  // "100dvh"
 * ```
 */
val Number.dvh: String
  get() = "${this}dvh"
