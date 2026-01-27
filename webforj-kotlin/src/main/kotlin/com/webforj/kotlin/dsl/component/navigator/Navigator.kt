package com.webforj.kotlin.dsl.component.navigator

import com.webforj.component.navigator.Navigator
import com.webforj.concern.HasComponents
import com.webforj.data.Paginator
import com.webforj.data.repository.Repository
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Navigator` with optional [text], [layout], [pageSize], [repository] and / or [totalItems].
 * ```
 * ... {
 *   navigator() // Empty Navigator component
 *   navigator("text", Navigator.Layout.PREVIEW, 5, repository) // Navigator using a Repository
 *   navigator("text", Navigator.Layout.PREVIEW, 5, totalItems = 10) // Navigator using totalItms
 * }
 * ```
 *
 * @param text The text to add to the `Navigator`.
 * @param layout The layout of the `Navigator`.
 * @param pageSize The number of items for each page of the `Navigator`.
 * @param repository The items to display in the `Navigator`, takes precedence over [totalItems].
 * @param totalItems The total items of the `Navigator`, ignored if [repository] is set.
 * @param block The initialization steps of the `Navigator`.
 * @return The configured `Navigator` instance.
 * @see Navigator
 * @see Repository
 * @see [paginator]
 */
fun @WebforjDsl HasComponents.navigator(
  text: String? = null,
  layout: Navigator.Layout? = null,
  pageSize: Int? = null,
  repository: Repository<*>? = null,
  totalItems: Int? = null,
  block: @WebforjDsl Navigator.() -> Unit = {}
): Navigator {
  val navigator = if (repository != null) {
    val temp = if (layout != null && pageSize != null) {
      Navigator(repository, pageSize, layout)
    } else if (pageSize != null) {
      Navigator(repository, pageSize)
    } else if (layout != null) {
      Navigator(repository, layout)
    } else {
      Navigator(repository)
    }
    temp.apply { text?.let { setText(it) } }
  } else if (totalItems != null) {
    val temp = if (layout != null && pageSize != null) {
      Navigator(totalItems, pageSize, layout)
    } else if (pageSize != null) {
      Navigator(totalItems, pageSize)
    } else if (layout != null) {
      Navigator(totalItems, layout)
    } else {
      Navigator(totalItems)
    }
    temp.apply { text?.let { setText(it) } }
  } else if (text != null) {
    Navigator(text).apply {
      pageSize?.let { paginator.setSize(pageSize) }
      layout?.let { setLayout(it) }
    }
  } else {
    Navigator().apply {
      pageSize?.let { paginator.setSize(pageSize) }
      layout?.let { setLayout(it) }
    }
  }
  return init(navigator, block)
}

/**
 * Creates the `Paginator` for a [Navigator] with an optional [repository], [pageSize] and / or [totalItems].
 * ```
 * navigator {
 *   paginator() // Empty Paginator component
 *   paginator(repository, 5) // Paginator with repository
 *   paginator(pageSize = 5, totalItems = 10) // Paginator with totalSize
 * }
 * ```
 *
 * @param repository The items to display in the `Navigator`, used to calculate pagination, takes precedence over [totalItems].
 * @param pageSize The number of items per page of the `Navigator`.
 * @param totalItems The total items to display in the `Navigator, ignored if [repository] is set.
 * @param block The initialization steps of the `Paginator`.
 * @return The configured `Paginator` instance.
 * @see Paginator
 * @see Repository
 * @see [navigator]
 */
fun @WebforjDsl Navigator.paginator(
  repository: Repository<*>? = null,
  pageSize: Int? = null,
  totalItems: Int? = null,
  block: @WebforjDsl Paginator.() -> Unit = {}
): Paginator {
  val paginator = if (repository != null) {
    if (pageSize != null) {
      Paginator(repository, pageSize)
    } else {
      Paginator(repository)
    }
  } else if (totalItems != null) {
    pageSize?.let { Paginator(totalItems, pageSize) } ?: Paginator(totalItems)
  } else {
    paginator.apply {
      size = pageSize ?: size
    }
  }
  paginator.block()
  setPaginator(paginator)
  return paginator
}
