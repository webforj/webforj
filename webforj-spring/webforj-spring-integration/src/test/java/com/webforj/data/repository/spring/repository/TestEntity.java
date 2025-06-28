package com.webforj.data.repository.spring.repository;

import com.webforj.data.HasEntityKey;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;

/**
 * Test entity class for repository testing.
 */
@Entity
@Table(name = "test_entity")
public class TestEntity implements HasEntityKey {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private String name;
  private String email;
  private Integer age;
  private Boolean active;
  private LocalDate birthDate;

  public TestEntity() {}

  public TestEntity(String name, String email, Integer age, Boolean active, LocalDate birthDate) {
    this.name = name;
    this.email = email;
    this.age = age;
    this.active = active;
    this.birthDate = birthDate;
  }

  public TestEntity(Long id, String name, String email, Integer age, Boolean active,
      LocalDate birthDate) {
    this.id = id;
    this.name = name;
    this.email = email;
    this.age = age;
    this.active = active;
    this.birthDate = birthDate;
  }

  @Override
  public Object getEntityKey() {
    return id;
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public Integer getAge() {
    return age;
  }

  public void setAge(Integer age) {
    this.age = age;
  }

  public Boolean getActive() {
    return active;
  }

  public boolean isActive() {
    return Boolean.TRUE.equals(active);
  }

  public void setActive(Boolean active) {
    this.active = active;
  }

  public LocalDate getBirthDate() {
    return birthDate;
  }

  public void setBirthDate(LocalDate birthDate) {
    this.birthDate = birthDate;
  }
}
