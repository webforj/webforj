package com.webforj.data;

import jakarta.validation.constraints.NotNull;

public class PersonBean {

  private String name;

  @NotNull
  private int age;

  public PersonBean(String name, int age) {
    setName(name);
    setAge(age);
  }

  public PersonBean() {
    this("", 0);
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setAge(int age) {
    this.age = age;
  }

  public int getAge() {
    return age;
  }
}
