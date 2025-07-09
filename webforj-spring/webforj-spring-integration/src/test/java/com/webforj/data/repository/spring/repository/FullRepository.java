package com.webforj.data.repository.spring.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

/**
 * Full repository interface for TestEntity.
 */
public interface FullRepository
    extends JpaRepository<TestEntity, Long>, JpaSpecificationExecutor<TestEntity> {
}
