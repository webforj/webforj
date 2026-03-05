package com.webforj.component.table.renderer;

import com.webforj.component.icons.IconDefinition;

class TestIconDefinition implements IconDefinition<TestIconDefinition> {
  private String name;
  private String pool;

  TestIconDefinition(String name, String pool) {
    this.name = name;
    this.pool = pool;
  }

  @Override
  public TestIconDefinition setName(String name) {
    this.name = name;
    return this;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public TestIconDefinition setPool(String pool) {
    this.pool = pool;
    return this;
  }

  @Override
  public String getPool() {
    return pool;
  }
}
