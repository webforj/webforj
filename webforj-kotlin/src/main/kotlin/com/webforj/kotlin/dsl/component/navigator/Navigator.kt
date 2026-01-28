package com.webforj.kotlin.dsl.component.navigator

import com.webforj.component.navigator.Navigator
import com.webforj.concern.HasComponents
import com.webforj.data.Paginator
import com.webforj.data.repository.Repository
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Navigator` with optional [text], [layout], [pageSize] and / or [totalItems].
 * ```
 * ... {
 *   navigator() // Empty Navigator component
 *   navigator("text", Navigator.Layout.PREVIEW, 5, 10) // Navigator using totalItms
 * }
 * ```
 *
 * @param text The text to add to the `Navigator`.
 * @param layout The layout of the `Navigator`.
 * @param pageSize The number of items for each page of the `Navigator`.
 * @param totalItems The total items of the `Navigator`.
 * @param block The initialization steps of the `Navigator`.
 * @return The configured `Navigator` instance.
 * @see Navigator
 * @see [paginator]
 */
fun @WebforjDsl HasComponents.navigator(
  text: String? = null,
  layout: Navigator.Layout? = null,
  pageSize: Int? = null,
  totalItems: Int? = null,
  block: @WebforjDsl Navigator.() -> Unit = {}
): Navigator {
  val navigator = when {
    text != null -> Navigator(text).apply {
      pageSize?.let { paginator.size = it }
      layout?.let { setLayout(it) }
    }
    layout != null && pageSize != null && totalItems != null ->
      Navigator(totalItems, pageSize, layout).apply {
      text?.let { setText(it) }
    }
    layout != null && totalItems != null -> Navigator(totalItems, layout).apply {
      text?.let { setText(it) }
    }
    pageSize != null && totalItems != null -> Navigator(totalItems, pageSize).apply {
      text?.let { setText(it) }
    }
    totalItems != null -> Navigator(totalItems).apply {
      text?.let { setText(it) }
    }
    else -> Navigator().apply {
      pageSize?.let { paginator.size = it }
      layout?.let { setLayout(it) }
    }
  }
  return init(navigator, block)
}

/**
 * Creates a `Navigator` with optional [text], [layout], [pageSize] and / or a mandatory [repository].
 * ```
 * ... {
 *   navigator() // Empty Navigator component
 *   navigator("text", Navigator.Layout.PREVIEW, 5, repository) // Navigator using a Repository
 * }
 * ```
 *
 * @param text The text to add to the `Navigator`.
 * @param layout The layout of the `Navigator`.
 * @param pageSize The number of items for each page of the `Navigator`.
 * @param repository The items to display in the `Navigator`.
 * @param block The initialization steps of the `Navigator`.
 * @return The configured `Navigator` instance.
 * @see Navigator
 * @see Repository
 * @see [paginator]
 */
fun @WebforjDsl HasComponents.navigator(
  repository: Repository<*>,
  text: String? = null,
  layout: Navigator.Layout? = null,
  pageSize: Int? = null,
  block: @WebforjDsl Navigator.() -> Unit = {}
): Navigator {
  val navigator = when {
    layout != null && pageSize != null -> Navigator(repository, pageSize, layout).apply {
        text?.let { setText(it) }
      }
    layout != null -> Navigator(repository, layout).apply {
      text?.let { setText(it) }
    }
    pageSize != null -> Navigator(repository, pageSize).apply {
      text?.let { setText(it) }
    }
    else -> Navigator(repository).apply {
      pageSize?.let { paginator.size = it }
      layout?.let { setLayout(it) }
    }
  }
  return init(navigator, block)
}

/**
 * Creates the `Paginator` for a [Navigator] with an optional [pageSize] and / or [totalItems].
 * ```
 * navigator {
 *   paginator() // Empty Paginator component
 *   paginator(5) // Paginator with pageSize
 *   paginator(5, 10) // Paginator with totalSize
 * }
 * ```
 *
 * @param pageSize The number of items per page of the `Navigator`.
 * @param totalItems The total items to display in the `Navigator.
 * @param block The initialization steps of the `Paginator`.
 * @return The configured `Paginator` instance.
 * @see Paginator
 * @see Repository
 * @see [navigator]
 */
fun @WebforjDsl Navigator.paginator(
  pageSize: Int? = null,
  totalItems: Int? = null,
  block: @WebforjDsl Paginator.() -> Unit = {}
): Paginator {
  val paginator = when {
    pageSize != null && totalItems != null -> Paginator(totalItems, pageSize)
    totalItems != null -> Paginator(totalItems)
    else -> paginator.apply {
      pageSize?.let { size = it }
    }
  }
  paginator.block()
  setPaginator(paginator)
  return paginator
}

/**
 * Creates the `Paginator` for a [Navigator] with a [repository] and an optional [pageSize].
 * ```
 * navigator {
 *   paginator() // Empty Paginator component
 *   paginator(repository, 5) // Paginator with repository
 * }
 * ```
 *
 * @param repository The items to display in the `Navigator`, used to calculate pagination.
 * @param pageSize The number of items per page of the `Navigator`.
 * @param block The initialization steps of the `Paginator`.
 * @return The configured `Paginator` instance.
 * @see Paginator
 * @see Repository
 * @see [navigator]
 */
fun @WebforjDsl Navigator.paginator(
  repository: Repository<*>,
  pageSize: Int? = null,
  block: @WebforjDsl Paginator.() -> Unit = {}
): Paginator {
  val paginator = pageSize?.let { Paginator(repository, pageSize) } ?: Paginator(repository)
  paginator.block()
  setPaginator(paginator)
  return paginator
}
