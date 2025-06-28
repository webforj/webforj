package com.webforj.data.repository.spring.repository;

import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * Test repository with only PagingAndSortingRepository interface.
 */
public interface PagingOnlyRepository extends PagingAndSortingRepository<TestEntity, Long> {
}
