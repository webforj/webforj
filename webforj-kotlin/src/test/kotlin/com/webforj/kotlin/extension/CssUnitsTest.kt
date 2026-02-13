package com.webforj.kotlin.extension

import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import org.junit.jupiter.params.provider.ValueSource
import kotlin.test.assertEquals

class CssUnitsTest {

  @ParameterizedTest(name = "should convert {0} to pixels")
  @ValueSource(ints = [0, 1, 10, 100, -5, -2])
  fun shouldConvertToPixels(value: Number) {
    val expected = "${value}px"
    assertEquals(expected, value.px)
  }

  @ParameterizedTest(name = "should convert {0} to centimeters")
  @ValueSource(doubles = [0.0, 1.0, 5.5, 10.0])
  fun shouldConvertToCentimeters(value: Number) {
    val expected = "${value}cm"
    assertEquals(expected, value.cm)
  }

  @ParameterizedTest(name = "should convert {0} to millimeters")
  @ValueSource(doubles = [0.0, 1.0, 5.5, 10.0])
  fun shouldConvertToMillimeters(value: Number) {
    val expected = "${value}mm"
    assertEquals(expected, value.mm)
  }

  @ParameterizedTest(name = "should convert {0} to inches")
  @ValueSource(doubles = [0.0, 1.0, 2.5, 10.0])
  fun shouldConvertToInches(value: Number) {
    val expected = "${value}in"
    assertEquals(expected, value.inches)
  }

  @ParameterizedTest(name = "should convert {0} to points")
  @ValueSource(ints = [0, 6, 12, 24])
  fun shouldConvertToPoints(value: Number) {
    val expected = "${value}pt"
    assertEquals(expected, value.pt)
  }

  @ParameterizedTest(name = "should convert {0} to picas")
  @ValueSource(ints = [0, 1, 3, 6])
  fun shouldConvertToPicas(value: Number) {
    val expected = "${value}pc"
    assertEquals(expected, value.pc)
  }

  @ParameterizedTest(name = "should convert {0} to em units")
  @ValueSource(doubles = [0.5, 1.0, 1.5, 2.0])
  fun shouldConvertToEm(value: Number) {
    val expected = "${value}em"
    assertEquals(expected, value.em)
  }

  @ParameterizedTest(name = "should convert {0} to rem units")
  @ValueSource(doubles = [0.5, 1.0, 1.2, 2.0])
  fun shouldConvertToRem(value: Number) {
    val expected = "${value}rem"
    assertEquals(expected, value.rem)
  }

  @ParameterizedTest(name = "should convert {0} to ex units")
  @ValueSource(doubles = [0.5, 1.0, 1.5, 2.0])
  fun shouldConvertToEx(value: Number) {
    val expected = "${value}ex"
    assertEquals(expected, value.ex)
  }

  @ParameterizedTest(name = "should convert {0} to ch units")
  @ValueSource(ints = [10, 20, 30, 40])
  fun shouldConvertToCh(value: Number) {
    val expected = "${value}ch"
    assertEquals(expected, value.ch)
  }

  @ParameterizedTest(name = "should convert {0} to percentage")
  @ValueSource(ints = [0, 25, 50, 75, 100])
  fun shouldConvertToPercentage(value: Number) {
    val expected = "${value}%"
    assertEquals(expected, value.percent)
  }

  @ParameterizedTest(name = "should convert {0} to viewport width")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToVw(value: Number) {
    val expected = "${value}vw"
    assertEquals(expected, value.vw)
  }

  @ParameterizedTest(name = "should convert {0} to viewport height")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToVh(value: Number) {
    val expected = "${value}vh"
    assertEquals(expected, value.vh)
  }

  @ParameterizedTest(name = "should convert {0} to viewport minimum")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToVmin(value: Number) {
    val expected = "${value}vmin"
    assertEquals(expected, value.vmin)
  }

  @ParameterizedTest(name = "should convert {0} to viewport maximum")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToVmax(value: Number) {
    val expected = "${value}vmax"
    assertEquals(expected, value.vmax)
  }

  @ParameterizedTest(name = "should convert {0} to small viewport width")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToSvw(value: Number) {
    val expected = "${value}svw"
    assertEquals(expected, value.svw)
  }

  @ParameterizedTest(name = "should convert {0} to small viewport height")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToSvh(value: Number) {
    val expected = "${value}svh"
    assertEquals(expected, value.svh)
  }

  @ParameterizedTest(name = "should convert {0} to large viewport width")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToLvw(value: Number) {
    val expected = "${value}lvw"
    assertEquals(expected, value.lvw)
  }

  @ParameterizedTest(name = "should convert {0} to large viewport height")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToLvh(value: Number) {
    val expected = "${value}lvh"
    assertEquals(expected, value.lvh)
  }

  @ParameterizedTest(name = "should convert {0} to dynamic viewport width")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToDvw(value: Number) {
    val expected = "${value}dvw"
    assertEquals(expected, value.dvw)
  }

  @ParameterizedTest(name = "should convert {0} to dynamic viewport height")
  @ValueSource(ints = [25, 50, 75, 100])
  fun shouldConvertToDvh(value: Number) {
    val expected = "${value}dvh"
    assertEquals(expected, value.dvh)
  }

  @Test
  @DisplayName("should handle decimal values correctly")
  fun shouldHandleDecimalValues() {
    assertEquals("1.5px", 1.5.px)
    assertEquals("2.75em", 2.75.em)
    assertEquals("50.5%", 50.5.percent)
  }

  @Test
  @DisplayName("should handle negative values correctly")
  fun shouldHandleNegativeValues() {
    assertEquals("-10px", (-10).px)
    assertEquals("-1.5em", (-1.5).em)
    assertEquals("-25%", (-25).percent)
  }

  @Test
  @DisplayName("should handle zero correctly")
  fun shouldHandleZero() {
    assertEquals("0px", 0.px)
    assertEquals("0em", 0.em)
    assertEquals("0%", 0.percent)
  }
}
