package com.webforj.kotlin.extension

import com.webforj.data.repository.CollectionRepository
import com.webforj.data.repository.Repository
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.random.Random
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class RepositoryExtensionsTest {
  lateinit var collection: MutableCollection<Int>
  lateinit var repository: Repository<Int>

  @BeforeEach
  fun setup() {
    val rng = Random(this.hashCode())
    collection = arrayListOf()
    repeat(100) { collection += rng.nextInt(1, Int.MAX_VALUE) }
    repository = CollectionRepository(collection)
    repository.onCommit {
      collection += it.commits
    }
  }

  @Test
  @DisplayName("Retrieve the entity with get operator")
  fun shouldFindEntityWithGetOperator() {
    val expected = collection.random();
    val result = repository[expected]
    assertTrue { result.filter { it == expected }.isPresent }
  }

  @Test
  @DisplayName("Does not retrieve entity with get operator if missing")
  fun shouldNotFindEntityWithGetOperator() {
    val unexpected = -collection.random()
    val result = repository[unexpected]
    assertTrue { result.isEmpty }
  }

  @Test
  @DisplayName("Returns true if entity is part of the repository")
  fun shouldReturnTrueForEntity() {
    assertTrue { collection.contains(collection.random()) }
  }

  @Test
  @DisplayName("Returns false if the entity is not part of the repository")
  fun shouldReturnFalseForMissingEntity() {
    assertFalse { -collection.random() in repository }
  }

  @Test
  @DisplayName("Add entity to repository with plus operator")
  fun shouldAddEntityToRepositoryWithPlusOperator() {
    assertFalse { 0 in repository }
    repository + 0
    assertTrue { 0 in repository }
  }

  @Test
  @DisplayName("Plus operator can be chained")
  fun shouldAllowPlusOperatorChaining() {
    assertFalse { 0 in repository }
    assertFalse { Int.MAX_VALUE in repository }
    repository + 0 + Int.MAX_VALUE
    assertTrue { Int.MAX_VALUE in repository }
  }


  @Test
  @DisplayName("Add entity to repository with plus assign operator")
  fun shouldAddEntityToRepositoryWithPlusAssignOperator() {
    val r = repository
    assertFalse { 0 in repository }
    r += 0
    assertTrue { 0 in repository }
  }

}
