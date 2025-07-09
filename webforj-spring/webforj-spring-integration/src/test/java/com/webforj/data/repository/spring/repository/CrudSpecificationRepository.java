package com.webforj.data.repository.spring.repository;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * Test repository with CrudRepository and JpaSpecificationExecutor interfaces.
 */
public interface CrudSpecificationRepository
    extends CrudRepository<TestEntity, Long>, JpaSpecificationExecutor<TestEntity> {
}
