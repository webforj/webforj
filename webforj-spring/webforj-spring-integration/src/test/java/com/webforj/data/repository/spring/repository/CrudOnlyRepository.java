package com.webforj.data.repository.spring.repository;

import org.springframework.data.repository.CrudRepository;

/**
 * Test repository with only CrudRepository interface.
 */
public interface CrudOnlyRepository extends CrudRepository<TestEntity, Long> {
}
