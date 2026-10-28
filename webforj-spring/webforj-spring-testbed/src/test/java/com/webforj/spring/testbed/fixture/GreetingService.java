package com.webforj.spring.testbed.fixture;

import org.springframework.stereotype.Service;

/** Provides a greeting for Spring dependency injection tests. */
@Service
public class GreetingService {

  public String greet() {
    return "Hello from Spring";
  }
}
