package com.webforj.data.repository.spring.repository;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.Repository;

/**
 * Test repository with only JpaSpecificationExecutor interface.
 */
public interface SpecificationOnlyRepository
    extends Repository<TestEntity, Long>, JpaSpecificationExecutor<TestEntity> {
}
