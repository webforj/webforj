package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasRestoreValueTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetValue() {
    Double expectedValue = 1.99;
    assertSame(component, component.setRestoreValue(expectedValue));
    assertEquals(expectedValue, component.getRestoreValue());

    component.setValue(2.99);
    component.restoreValue();
    assertEquals(expectedValue, component.getValue());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasRestoreValue<CompositeMock, Double>, HasValue<CompositeMock, Double> {

    @Override
    public ListenerRegistration<ValueChangeEvent<Double>> addValueChangeListener(
        EventListener<ValueChangeEvent<Double>> listener) {
      throw new UnsupportedOperationException("Unimplemented method 'addValueChangeListener'");
    }
  }
}
