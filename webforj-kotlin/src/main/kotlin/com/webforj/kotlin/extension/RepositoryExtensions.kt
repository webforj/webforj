package com.webforj.kotlin.extension

import com.webforj.data.repository.Repository
import java.util.Optional

/**
 * Provides the get operator for [Repository] implementations so that the squared brackets (`[key]` ) can be used
 * to retrieve an entity from it.
 * ```
 * val repository = // Initialization
 * val result = repository[key]
 * ```
 *
 * @param key The key for the entity.
 * @return An [Optional] with the entity if present.
 */
operator fun <T> Repository<T>.get(key: Any?): Optional<T> = find(key)

/**
 * Provides the contains operator for [Repository] implementations so the `in` and `!in` can be used to
 * check for the presence or absence of entities.
 * ```
 * val repository = // Initialization
 * val present = entity in repository
 * val absent = entity !in repository
 * ```
 *
 * @param entity The member of the [Repository] to check for presence or absence.
 * @return `true` if the [entity] is part of the [Repository], else false, for `in`, flipped for `!in`.
 */
operator fun <T> Repository<T>.contains(entity: T): Boolean = has(entity)

/**
 * Provides the plus operator for [Repository] implementations so that `+` can be used to add entities.
 * ```
 * val repository = // Initialization
 * repository + entity1 + entity2
 * ```
 * **Note**: This operator can be chained to add multiple entities one by one.
 *
 * @param entity The member to add to the [Repository].
 * @return `this` [Repository] instance.
 */
operator fun <T> Repository<T>.plus(entity: T): Repository<T> = commit(entity)

/**
 * Provides the plus assign operator for [Repository] implementations so that `+=` can be used to add entities.
 * ```
 * val repository = // Initialization
 * repository += entity
 * ```
 *
 * @param entity The member to add to the [Repository]
 */
operator fun <T> Repository<T>.plusAssign(entity: T) {
  commit(entity)
}
