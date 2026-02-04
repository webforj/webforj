package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.Component
import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.concern.HasText
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class ElementsTest {

  companion object Providers {

    fun createTestValue(name: String, block: HasComponents.(String) -> Any): Array<Any> {
      val root = Div()
      return arrayOf(name, root, root.block(name))
    }

    @JvmStatic
    fun provideEmptyElements(): List<Array<Any>> {
      return listOf(
        createTestValue("Anchor") { anchor() },
        createTestValue("Article") { article() },
        createTestValue("Aside") { aside() },
        createTestValue("Break") { `break`() },
        createTestValue("Div") { div() },
        createTestValue("Emphasis") { emphasis() },
        createTestValue("Footer") { nativeFooter() },
        createTestValue("Fieldset") { fieldset() },
        createTestValue("FormattedText") { formattedText() },
        createTestValue("Header") { nativeHeader() },
        createTestValue("H1") { h1() },
        createTestValue("H2") { h2() },
        createTestValue("H3") { h3() },
        createTestValue("H4") { h4() },
        createTestValue("H5") { h5() },
        createTestValue("H6") { h6() },
        createTestValue("Iframe") { iframe() },
        createTestValue("Img") { img() },
        createTestValue("Main") { main() },
        createTestValue("NativeButton") { nativeButton() },
        createTestValue("Nav") { nav() },
        createTestValue("OrderedList") { orderedList() },
        createTestValue("Paragraph") { paragraph() },
        createTestValue("Section") { section() },
        createTestValue("Span") { span() },
        createTestValue("Strong") { strong() },
      )
    }

    @JvmStatic
    fun provideElementsWithText(): List<Array<Any>> {
      return listOf(
        createTestValue("Anchor") { anchor(text = it) },
        createTestValue("Article") { article(it) },
        createTestValue("Aside") { aside(it) },
        createTestValue("Div") { div(it) },
        createTestValue("Emphasis") { emphasis(it) },
        createTestValue("Footer") { nativeFooter(it) },
        createTestValue("FormattedText") { formattedText(it) },
        createTestValue("Header") { nativeHeader(it) },
        createTestValue("H1") { h1(it) },
        createTestValue("H2") { h2(it) },
        createTestValue("H3") { h3(it) },
        createTestValue("H4") { h4(it) },
        createTestValue("H5") { h5(it) },
        createTestValue("H6") { h6(it) },
        createTestValue("Main") { main(it) },
        createTestValue("NativeButton") { nativeButton(it) },
        createTestValue("Nav") { nav(it) },
        createTestValue("OrderedList") { orderedList(it) },
        createTestValue("Paragraph") { paragraph(it) },
        createTestValue("Section") { section(it) },
        createTestValue("Span") { span(it) },
        createTestValue("Strong") { strong(it) },
        createTestValue("UnorderedList") { unorderedList(it) },
      )
    }

    @JvmStatic
    fun provideElementsWithBlock(): List<Array<Any>> {
      return listOf(
        createTestValue("Anchor") { anchor { name = it } },
        createTestValue("Article") { article { name = it } },
        createTestValue("Aside") { aside { name = it } },
        createTestValue("Break") { `break` { name = it } },
        createTestValue("Div") { div { name = it} },
        createTestValue("Emphasis") { emphasis { name = it } },
        createTestValue("Footer") { nativeFooter { name = it } },
        createTestValue("Fieldset") { fieldset { name = it } },
        createTestValue("FormattedText") { formattedText { name = it} },
        createTestValue("Header") { nativeHeader { name = it} },
        createTestValue("H1") { h1 { name = it } },
        createTestValue("H2") { h2 { name = it } },
        createTestValue("H3") { h3 { name = it } },
        createTestValue("H4") { h4 { name = it } },
        createTestValue("H5") { h5 { name = it } },
        createTestValue("H6") { h6 { name = it } },
        createTestValue("Iframe") { iframe { name = it } },
        createTestValue("Img") { img { name = it } },
        createTestValue("Main") { main { name = it} },
        createTestValue("NativeButton") { nativeButton { name = it } },
        createTestValue("Nav") { nav { name = it } },
        createTestValue("OrderedList") { orderedList { name = it } },
        createTestValue("Paragraph") { paragraph { name = it } },
        createTestValue("Section") { section { name = it } },
        createTestValue("Span") { span { name = it } },
        createTestValue("Strong") { strong { name = it } },
        createTestValue("UnorderedList") { unorderedList { name = it } },
      )
    }

    @JvmStatic
    fun provideElementsWithTextAndBlock(): List<Array<Any>> {
      return listOf(
        createTestValue("Anchor") { anchor(text = it) { name = it } },
        createTestValue("Article") { article(it) { name = it } },
        createTestValue("Aside") { aside(it) { name = it } },
        createTestValue("Div") { div(it) { name = it} },
        createTestValue("Emphasis") { emphasis(it) { name = it } },
        createTestValue("Footer") { nativeFooter(it) { name = it } },
        createTestValue("FormattedText") { formattedText(it) { name = it} },
        createTestValue("Header") { nativeHeader(it) { name = it} },
        createTestValue("H1") { h1(it) { name = it } },
        createTestValue("H2") { h2(it) { name = it } },
        createTestValue("H3") { h3(it) { name = it } },
        createTestValue("H4") { h4(it) { name = it } },
        createTestValue("H5") { h5(it) { name = it } },
        createTestValue("H6") { h6(it) { name = it } },
        createTestValue("Main") { main(it) { name = it} },
        createTestValue("NativeButton") { nativeButton(it) { name = it } },
        createTestValue("Nav") { nav(it) { name = it } },
        createTestValue("OrderedList") { orderedList(it) { name = it } },
        createTestValue("Paragraph") { paragraph(it) { name = it } },
        createTestValue("Section") { section(it) { name = it } },
        createTestValue("Span") { span(it) { name = it } },
        createTestValue("Strong") { strong(it) { name = it } },
        createTestValue("UnorderedList") { unorderedList(it) { name = it } },
      )
    }

  }

  @ParameterizedTest(name = "Create empty {0} element.")
  @MethodSource("provideEmptyElements")
  fun shouldCreateEmptyElement(name: String, root: HasComponents, element: HasText<*>) {
    assertNotNull(element)
    assertEquals("", element.text)
    element as Component
    assertTrue { root.hasComponent(element) }
  }

  @ParameterizedTest(name = "Create {0} element with text")
  @MethodSource("provideElementsWithText")
  fun shouldCreateElementWithText(name: String, root: HasComponents, element: HasText<*>) {
    assertNotNull(element)
    assertEquals(name, element.text)
    element as Component
    assertTrue { root.hasComponent(element) }
  }

  @ParameterizedTest(name = "Create {0} element with block")
  @MethodSource("provideElementsWithBlock")
  fun shouldCreateElementWithBlock(name: String, root: HasComponents, element: Component) {
    assertNotNull(element)
    assertEquals(name, element.name)
    assertTrue { root.hasComponent(element) }
  }

  @ParameterizedTest(name = "Create {0} element with text and block")
  @MethodSource("provideElementsWithTextAndBlock")
  fun shouldCreateElementWithTextAndBlock(name: String, root: HasComponents, element: HasText<*>) {
    assertNotNull(element)
    assertEquals(name, element.text)
    element as Component
    assertEquals(name, element.name)
    assertTrue { root.hasComponent(element) }
  }

}
