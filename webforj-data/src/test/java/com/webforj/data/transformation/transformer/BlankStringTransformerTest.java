package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.binding.BindingContext;
import com.webforj.data.validation.server.ValidationResult;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

class BlankStringTransformerTest {

  private final BlankStringTransformer transformer = new BlankStringTransformer();

  @Test
  void shouldTransformToModel() {
    assertEquals(" Review the plan ", transformer.transformToModel(" Review the plan "));
  }

  @Test
  void shouldTransformToComponent() {
    assertEquals("Review the plan", transformer.transformToComponent("Review the plan"));
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = {"", "   "})
  void shouldTransformBlankTextToNull(String viewValue) {
    assertNull(transformer.transformToModel(viewValue));
  }

  @Test
  void shouldTransformNullToEmptyText() {
    assertEquals("", transformer.transformToComponent(null));
  }

  @Test
  void shouldValidateAndWriteNullForBlankText() {
    TextComponentMock component = new TextComponentMock();
    BindingContext<TaskBean> context = new BindingContext<>(TaskBean.class);
    context.bind(component, "note", String.class).useTransformer(transformer)
        .useValidator(value -> value == null || value.length() >= 5, "Too short").add();
    TaskBean bean = new TaskBean();
    component.setValue("   ");

    assertTrue(context.write(bean).isValid());
    assertNull(bean.getNote());
  }

  @Test
  void shouldValidateTextThatIsNotBlank() {
    TextComponentMock component = new TextComponentMock();
    BindingContext<TaskBean> context = new BindingContext<>(TaskBean.class);
    context.bind(component, "note", String.class).useTransformer(transformer)
        .useValidator(value -> value == null || value.length() >= 5, "Too short").add();
    TaskBean bean = new TaskBean();
    component.setValue("Fix");

    ValidationResult result = context.write(bean);

    assertFalse(result.isValid());
    assertEquals(List.of("Too short"), result.getMessages());
    assertEquals("Review", bean.getNote());
  }

  @Test
  void shouldReadNullAsEmptyText() {
    TextComponentMock component = new TextComponentMock();
    BindingContext<TaskBean> context = new BindingContext<>(TaskBean.class);
    context.bind(component, "note", String.class).useTransformer(transformer).add();
    TaskBean bean = new TaskBean();
    bean.setNote(null);
    component.setValue("Review");

    context.read(bean);

    assertEquals("", component.getValue());
  }
}
