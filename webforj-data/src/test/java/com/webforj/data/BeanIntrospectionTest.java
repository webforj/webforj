package com.webforj.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.beans.IntrospectionException;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BeanIntrospectionTest {

  @Retention(RetentionPolicy.RUNTIME)
  @Target({ElementType.FIELD, ElementType.METHOD})
  @interface Tag {
    String value();
  }

  static class FieldOnly {
    @Tag("field")
    private String name;

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }
  }

  static class GetterAnnotated {
    private int age;

    @Tag("getter")
    public int getAge() {
      return age;
    }

    public void setAge(int age) {
      this.age = age;
    }
  }

  static class SetterAnnotated {
    private boolean active;

    public boolean isActive() {
      return active;
    }

    @Tag("setter")
    public void setActive(boolean active) {
      this.active = active;
    }
  }

  static class Parent {
    @Tag("inherited")
    private String inherited;

    public String getInherited() {
      return inherited;
    }

    public void setInherited(String inherited) {
      this.inherited = inherited;
    }
  }

  static class Child extends Parent {
    private String own;

    public String getOwn() {
      return own;
    }

    public void setOwn(String own) {
      this.own = own;
    }
  }

  static class WriteOnly {
    private String secret;

    public void setSecret(String secret) {
      this.secret = secret;
    }
  }

  static class WithList {
    private List<String> tags = new ArrayList<>();

    public List<String> getTags() {
      return tags;
    }

    public void setTags(List<String> tags) {
      this.tags = tags;
    }
  }

  @Test
  void shouldReturnSameInstanceFromCache() throws IntrospectionException {
    BeanIntrospection a = BeanIntrospection.of(FieldOnly.class);
    BeanIntrospection b = BeanIntrospection.of(FieldOnly.class);
    assertSame(a, b);
  }

  @Test
  void shouldExposeBeanClass() throws IntrospectionException {
    assertEquals(FieldOnly.class, BeanIntrospection.of(FieldOnly.class).getBeanClass());
  }

  @Test
  void shouldReturnPropertyDescriptorForKnownProperty() throws IntrospectionException {
    var pd = BeanIntrospection.of(FieldOnly.class).getPropertyDescriptor("name");
    assertNotNull(pd);
    assertEquals("name", pd.getName());
    assertEquals(String.class, pd.getPropertyType());
  }

  @Test
  void shouldThrowIllegalArgumentForUnknownProperty() {
    var ex = assertThrows(IllegalArgumentException.class,
        () -> BeanIntrospection.of(FieldOnly.class).getPropertyDescriptor("missing"));
    assertTrue(ex.getMessage().contains("Property 'missing'"));
    assertTrue(ex.getMessage().contains(FieldOnly.class.getName()));
  }

  @Test
  void shouldReturnNullFromGetPropertyForUnknownProperty() throws IntrospectionException {
    assertNull(BeanIntrospection.of(FieldOnly.class).getProperty("missing"));
  }

  @Test
  void shouldSkipWriteOnlyProperties() throws IntrospectionException {
    var properties = BeanIntrospection.of(WriteOnly.class).getProperties();
    assertTrue(properties.isEmpty(), "write only property should be skipped, got: " + properties);
  }

  @Test
  void shouldAggregateFieldGetterAndSetterAnnotations() throws IntrospectionException {
    var ageProperty = BeanIntrospection.of(GetterAnnotated.class).getProperty("age");
    var activeProperty = BeanIntrospection.of(SetterAnnotated.class).getProperty("active");
    var nameProperty = BeanIntrospection.of(FieldOnly.class).getProperty("name");

    assertEquals("getter", ageProperty.annotations().stream().filter(Tag.class::isInstance)
        .map(Tag.class::cast).findFirst().orElseThrow().value());
    assertEquals("setter", activeProperty.annotations().stream().filter(Tag.class::isInstance)
        .map(Tag.class::cast).findFirst().orElseThrow().value());
    assertEquals("field", nameProperty.annotations().stream().filter(Tag.class::isInstance)
        .map(Tag.class::cast).findFirst().orElseThrow().value());
  }

  @Test
  void shouldFindInheritedFieldAnnotations() throws IntrospectionException {
    var inheritedProperty = BeanIntrospection.of(Child.class).getProperty("inherited");
    assertNotNull(inheritedProperty);
    assertEquals("inherited", inheritedProperty.annotations().stream().filter(Tag.class::isInstance)
        .map(Tag.class::cast).findFirst().orElseThrow().value());
  }

  @Test
  void shouldExposeListPropertyAsRegularDescriptor() throws Exception {
    var pd = BeanIntrospection.of(WithList.class).getPropertyDescriptor("tags");
    assertNotNull(pd);
    assertEquals(List.class, pd.getPropertyType());
    assertNotNull(pd.getReadMethod());
    assertNotNull(pd.getWriteMethod());

    WithList bean = new WithList();
    List<String> values = List.of("a", "b", "c");
    pd.getWriteMethod().invoke(bean, values);
    assertEquals(values, pd.getReadMethod().invoke(bean));
  }

  @Test
  void shouldBoxPrimitivePropertyType() throws IntrospectionException {
    assertEquals(Integer.class,
        BeanIntrospection.of(GetterAnnotated.class).getProperty("age").getPropertyType());
    assertEquals(Boolean.class,
        BeanIntrospection.of(SetterAnnotated.class).getProperty("active").getPropertyType());
  }

  @Test
  void shouldReturnPropertiesInDeclarationOrder() throws IntrospectionException {
    var names = BeanIntrospection.of(CompanyBean.class).getProperties().stream()
        .map(BeanIntrospection.Property::getName).toList();
    assertEquals(List.of("name", "address", "email", "displayName"), names);
  }

  @Test
  void shouldReturnInheritedPropertiesFirstInDeclarationOrder() throws IntrospectionException {
    var names = BeanIntrospection.of(Child.class).getProperties().stream()
        .map(BeanIntrospection.Property::getName).toList();
    assertEquals(List.of("inherited", "own"), names);
  }

  @Nested
  class PathAware {

    @Test
    void shouldResolveDottedPathToLeafProperty() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      BeanIntrospection.Property leaf = bi.getProperty("address.street");

      assertNotNull(leaf);
      assertEquals("address.street", leaf.getName());
      assertEquals(String.class, leaf.getPropertyType());
    }

    @Test
    void shouldExposeLeafAnnotationsThroughDottedPath() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      BeanIntrospection.Property street = bi.getProperty("address.street");
      BeanIntrospection.Property city = bi.getProperty("address.city");

      assertTrue(street.annotations().stream().anyMatch(NotNull.class::isInstance),
          "street should carry @NotNull from AddressBean.street");
      assertTrue(city.annotations().stream().anyMatch(Size.class::isInstance),
          "city should carry @Size from AddressBean.city");
    }

    @Test
    void shouldReturnNullForUnknownTopSegment() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      assertNull(bi.getProperty("naem"));
    }

    @Test
    void shouldReturnNullForUnknownLeafSegment() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      assertNull(bi.getProperty("address.naem"));
    }

    @Test
    void shouldReturnNullForPathThatStopsAtNonBeanType() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      assertNull(bi.getProperty("name.length"));
    }

    @Test
    void shouldReadFlatPropertyThroughIntrospection() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      CompanyBean bean = new CompanyBean("Acme", null, null);
      assertEquals("Acme", bi.getProperty("name").read(bean));
    }

    @Test
    void shouldReadDottedPathReturningLeafValue() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      CompanyBean bean =
          new CompanyBean("Acme", new AddressBean("Main", "Springfield", "12345"), null);

      assertEquals("Main", bi.getProperty("address.street").read(bean));
    }

    @Test
    void shouldReadDottedPathReturningNullWhenIntermediateIsNull() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      CompanyBean bean = new CompanyBean("Acme", null, null);

      assertNull(bi.getProperty("address.street").read(bean),
          "null intermediate must yield null on read");
      assertNull(bean.getAddress(), "read must not mutate the parent bean");
    }

    @Test
    void shouldWriteFlatPropertyThroughIntrospection() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      CompanyBean bean = new CompanyBean();
      bi.getProperty("name").write(bean, "Acme");
      assertEquals("Acme", bean.getName());
    }

    @Test
    void shouldWriteDottedPathSettingLeafValue() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      AddressBean addr = new AddressBean("Old", "Springfield", "12345");
      CompanyBean bean = new CompanyBean("Acme", addr, null);

      bi.getProperty("address.street").write(bean, "New");

      assertEquals("New", bean.getAddress().getStreet());
      assertSame(addr, bean.getAddress(),
          "write must not replace the existing intermediate when it is non null");
    }

    @Test
    void shouldInstantiateNullIntermediateOnDottedWrite() throws IntrospectionException {
      BeanIntrospection bi = BeanIntrospection.of(CompanyBean.class);
      CompanyBean bean = new CompanyBean("Acme", null, null);
      assertNull(bean.getAddress(), "precondition");

      bi.getProperty("address.street").write(bean, "Main");

      assertNotNull(bean.getAddress(), "write must instantiate the null intermediate");
      assertEquals("Main", bean.getAddress().getStreet());
    }
  }
}
