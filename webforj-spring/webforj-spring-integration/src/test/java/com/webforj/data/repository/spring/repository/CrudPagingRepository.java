package com.webforj.data.repository.spring.repository;

import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * Test repository with CrudRepository and PagingAndSortingRepository interfaces.
 */
public interface CrudPagingRepository
    extends CrudRepository<TestEntity, Long>, PagingAndSortingRepository<TestEntity, Long> {
}
