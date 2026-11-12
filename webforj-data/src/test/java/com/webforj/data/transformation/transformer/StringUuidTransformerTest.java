package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.binding.BindingContext;
import com.webforj.data.transformation.TransformationException;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

class StringUuidTransformerTest {

  private final StringUuidTransformer transformer = new StringUuidTransformer();

  @Test
  void shouldTransformToModel() {
    assertEquals(UUID.fromString("123e4567-e89b-12d3-a456-426614174000"),
        transformer.transformToModel(" 123E4567-E89B-12D3-A456-426614174000 "));
  }

  @Test
  void shouldTransformToComponent() {
    assertEquals("123e4567-e89b-12d3-a456-426614174000",
        transformer.transformToComponent(UUID.fromString("123e4567-e89b-12d3-a456-426614174000")));
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = {"", "   "})
  void shouldTransformEmptyTextToNull(String viewValue) {
    assertNull(transformer.transformToModel(viewValue));
  }

  @Test
  void shouldTransformNullToNull() {
    assertNull(transformer.transformToComponent(null));
  }

  @Test
  void shouldWriteNullToBeanForEmptyText() {
    TextComponentMock component = new TextComponentMock();
    BindingContext<TaskBean> context = new BindingContext<>(TaskBean.class);
    context.bind(component, "id", UUID.class).useTransformer(transformer).add();
    TaskBean bean = new TaskBean();
    component.setValue("");

    assertTrue(context.write(bean).isValid());
    assertNull(bean.getId());
  }

  @ParameterizedTest
  @ValueSource(strings = {"not a uuid", "1-1-1-1-1", "123e4567-e89b-12d3-a456-4266141740001"})
  void shouldRejectTextThatIsNotUuid(String viewValue) {
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(viewValue));
    assertEquals("The value must be a UUID such as 123e4567-e89b-12d3-a456-426614174000.",
        exception.getMessage());
  }
}
