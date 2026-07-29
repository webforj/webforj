package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GetBeanInfoActionTest {

  private final GetBeanInfoAction action = new GetBeanInfoAction();

  @Test
  @DisplayName("Should return properties with types and annotations")
  void shouldReturnPropertiesWithTypesAndAnnotations() {
    JsonObject params = new JsonObject();
    params.addProperty("className", Customer.class.getName());

    GetBeanInfoAction.Response response = action.handle(params);

    assertEquals(Customer.class.getName(), response.getClassName());
    Optional<GetBeanInfoAction.PropertyView> email = response.getProperties().stream()
        .filter(property -> property.getName().equals("email")).findFirst();
    assertTrue(email.isPresent());
    assertEquals(String.class.getName(), email.get().getType());
    assertTrue(email.get().getAnnotations().stream()
        .anyMatch(annotation -> annotation.contains("Deprecated")));
  }

  @Test
  @DisplayName("Should throw when className is missing")
  void shouldThrowWhenClassNameMissing() {
    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(new JsonObject()));
    assertTrue(ex.getMessage().contains("Missing className"));
  }

  @Test
  @DisplayName("Should throw when the class does not exist")
  void shouldThrowWhenClassNotFound() {
    JsonObject params = new JsonObject();
    params.addProperty("className", "com.example.DoesNotExist");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("Class not found"));
  }

  @Test
  @DisplayName("Should refuse platform classes")
  void shouldRefusePlatformClasses() {
    JsonObject params = new JsonObject();
    params.addProperty("className", "java.lang.String");

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("not an application class"));
  }

  public static class Customer {

    private String email;
    private int age;

    @Deprecated
    public String getEmail() {
      return email;
    }

    public void setEmail(String email) {
      this.email = email;
    }

    public int getAge() {
      return age;
    }

    public void setAge(int age) {
      this.age = age;
    }
  }
}
