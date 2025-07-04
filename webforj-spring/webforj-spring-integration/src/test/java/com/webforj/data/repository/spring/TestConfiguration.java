package com.webforj.data.repository.spring;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

/**
 * Test configuration for Spring Data JPA tests.
 */
@SpringBootApplication
@EnableJpaRepositories
@EntityScan
public class TestConfiguration {
}
