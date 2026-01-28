package com.webforj.kotlin.dsl.component.navigator

import com.webforj.component.html.elements.Div
import com.webforj.component.navigator.Navigator
import com.webforj.data.Paginator
import com.webforj.data.repository.Repository
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import org.mockito.kotlin.doReturn
import org.mockito.kotlin.mock

class NavigatorTest {
  lateinit var root: Div

  @BeforeEach
  fun setup() {
    root = Div()
  }

  companion object Providers {
    val repository = mock<Repository<*>> {
        on { size() } doReturn 20
    }

    @JvmStatic
    fun provideNavigatorsWithTextLayoutPageSizeAndRepository(): List<Array<Any?>> {
      val list = arrayListOf<Array<Any?>>()
      for (text in listOf("text", null)) {
        for (layout in listOf(Navigator.Layout.PAGES, null)) {
          for (pageSize in listOf(5, null)) {
            list.add(arrayOf(text, layout, pageSize, repository))
          }
        }
      }
      return list
    }

    @JvmStatic
    fun provideNavigatorsWithTextLayoutPageSizeAndTotalItems(): List<Array<Any?>> {
      val list = arrayListOf<Array<Any?>>()
      for (text in listOf("text", null)) {
        for (layout in listOf(Navigator.Layout.PAGES, null)) {
          for (pageSize in listOf(5, null)) {
            list.add(arrayOf(text, layout, pageSize, 10))
          }
        }
      }
      return list
    }

  }

  @ParameterizedTest(name = "Create Navigator with repository, text = {0}, layout = {1} and pageSize = {2}")
  @MethodSource("provideNavigatorsWithTextLayoutPageSizeAndRepository")
  fun shouldCreateNavigatorWithRepositoryTextLayoutAndPageSize(text: String?, layout: Navigator.Layout?, pageSize: Int?, repository: Repository<*>) {
    val navigator = root.navigator(repository, text, layout, pageSize)
      Assertions.assertTrue { root.hasComponent(navigator) }
      Assertions.assertEquals(text ?: "", navigator.text)
      Assertions.assertEquals(layout ?: Navigator.Layout.PREVIEW, navigator.layout)
      Assertions.assertEquals(pageSize ?: Paginator.DEFAULT_PAGE_SIZE, navigator.paginator.size)
      Assertions.assertEquals(repository.size(), navigator.paginator.totalItems)
      assertThrows<UnsupportedOperationException> { navigator.paginator.totalItems = 0 }
  }

  @ParameterizedTest(name = "Create Navigator with repository, text = {0}, layout = {1} and pageSize = {2}")
  @MethodSource("provideNavigatorsWithTextLayoutPageSizeAndRepository")
  fun shouldCreateNavigatorWithRepositoryTextLayoutPageSizeAndBlock(text: String?, layout: Navigator.Layout?, pageSize: Int?, repository: Repository<*>) {
    val expected = "Navigator"
    val navigator = root.navigator(repository, text, layout, pageSize) {
      name = expected
    }
      Assertions.assertTrue { root.hasComponent(navigator) }
      Assertions.assertEquals(text ?: "", navigator.text)
      Assertions.assertEquals(layout ?: Navigator.Layout.PREVIEW, navigator.layout)
      Assertions.assertEquals(pageSize ?: Paginator.DEFAULT_PAGE_SIZE, navigator.paginator.size)
      Assertions.assertEquals(repository.size(), navigator.paginator.totalItems)
      assertThrows<UnsupportedOperationException> { navigator.paginator.totalItems = 0 }
      Assertions.assertEquals(expected, navigator.name)
  }

  @ParameterizedTest(name = "Create Navigator with totalItems, text = {0}, layout = {1} and pageSize = {2}")
  @MethodSource("provideNavigatorsWithTextLayoutPageSizeAndTotalItems")
  fun shouldCreateNavigatorWithTotalItemsTextLayoutAndPageSize(text: String?, layout: Navigator.Layout?, pageSize: Int?, totalItems: Int) {
    val navigator = root.navigator(text, layout, pageSize, totalItems)
      Assertions.assertTrue { root.hasComponent(navigator) }
      Assertions.assertEquals(text ?: "", navigator.text)
      Assertions.assertEquals(layout ?: Navigator.Layout.PREVIEW, navigator.layout)
      Assertions.assertEquals(pageSize ?: Paginator.DEFAULT_PAGE_SIZE, navigator.paginator.size)
      Assertions.assertEquals(totalItems, navigator.paginator.totalItems)
      Assertions.assertDoesNotThrow { navigator.paginator.totalItems = 0 }
  }

  @ParameterizedTest(name = "Create Navigator with totalItems, text = {0}, layout = {1} and pageSize = {2}")
  @MethodSource("provideNavigatorsWithTextLayoutPageSizeAndTotalItems")
  fun shouldCreateNavigatorWithTotalItesmTextLayoutPageSizeAndBlock(text: String?, layout: Navigator.Layout?, pageSize: Int?, totalItems: Int) {
    val expected = "Navigator"
    val navigator = root.navigator(text, layout, pageSize, totalItems) {
      name = expected
    }
      Assertions.assertTrue { root.hasComponent(navigator) }
      Assertions.assertEquals(text ?: "", navigator.text)
      Assertions.assertEquals(layout ?: Navigator.Layout.PREVIEW, navigator.layout)
      Assertions.assertEquals(pageSize ?: Paginator.DEFAULT_PAGE_SIZE, navigator.paginator.size)
      Assertions.assertEquals(totalItems, navigator.paginator.totalItems)
      Assertions.assertDoesNotThrow { navigator.paginator.totalItems = 0 }
      Assertions.assertEquals(expected, navigator.name)
  }

  @ParameterizedTest(name = "Create Navigator with repository and pageSize = {2}")
  @MethodSource("provideNavigatorsWithTextLayoutPageSizeAndRepository")
  fun shouldCreatePaginatorWithRepositoryAndPageSize(text: String?, layout: Navigator.Layout?, pageSize: Int?, repository: Repository<*>) {
    val navigator = root.navigator(text, layout)
    val paginator = navigator.paginator(repository, pageSize)
      Assertions.assertTrue { root.hasComponent(navigator) }
      Assertions.assertEquals(paginator, navigator.paginator)
      Assertions.assertEquals(text ?: "", navigator.text)
      Assertions.assertEquals(layout ?: Navigator.Layout.PREVIEW, navigator.layout)
      Assertions.assertEquals(pageSize ?: Paginator.DEFAULT_PAGE_SIZE, paginator.size)
      Assertions.assertEquals(repository.size(), paginator.totalItems)
      assertThrows<UnsupportedOperationException> { paginator.totalItems = 0 }
  }

  @ParameterizedTest(name = "Create Navigator with totalItems and pageSize = {2}")
  @MethodSource("provideNavigatorsWithTextLayoutPageSizeAndTotalItems")
  fun shouldCreatePaginatorWithTotalItemsAndPageSize(text: String?, layout: Navigator.Layout?, pageSize: Int?, totalItems: Int) {
    val navigator = root.navigator(text, layout)
    val paginator = navigator.paginator(pageSize = pageSize, totalItems = totalItems)
      Assertions.assertTrue { root.hasComponent(navigator) }
      Assertions.assertEquals(paginator, navigator.paginator)
      Assertions.assertEquals(text ?: "", navigator.text)
      Assertions.assertEquals(layout ?: Navigator.Layout.PREVIEW, navigator.layout)
      Assertions.assertEquals(pageSize ?: Paginator.DEFAULT_PAGE_SIZE, paginator.size)
      Assertions.assertEquals(totalItems, paginator.totalItems)
      Assertions.assertDoesNotThrow { paginator.totalItems = 0 }
  }

}
