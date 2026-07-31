package com.webforj.component.card;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.Component;
import com.webforj.component.Expanse;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.element.event.ElementClickEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class CardTest {
  Card component;

  @BeforeEach
  void setUp() {
    component = new Card();
  }

  @Nested
  @DisplayName("Constructors")
  class Constructors {

    @Test
    void shouldCreateEmptyCardWithDefaults() {
      Card card = new Card();

      assertEquals(Card.Shadow.XSMALL, card.getShadow());
      assertEquals(Expanse.MEDIUM, card.getExpanse());
      assertEquals(Card.Orientation.VERTICAL, card.getOrientation());
      assertFalse(card.isDivided());
      assertFalse(card.isBorderless());
      assertEquals(0, card.getComponentCount());
    }

    @Test
    void shouldCreateCardWithBodyContent() {
      Component first = new Card();
      Component second = new Card();
      Card card = new Card(first, second);

      assertEquals(2, card.getComponentCount());
      assertTrue(card.hasComponent(first));
      assertTrue(card.hasComponent(second));
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Card.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToFigure() {
      Component figure = new Card();
      component.addToFigure(figure);

      assertEquals(figure, component.getOriginalElement().getFirstComponentInSlot("figure"));
    }

    @Test
    void shouldAddToIcon() {
      Component icon = new Card();
      component.addToIcon(icon);

      assertEquals(icon, component.getOriginalElement().getFirstComponentInSlot("icon"));
    }

    @Test
    void shouldAddToTitle() {
      Component title = new Card();
      component.addToTitle(title);

      assertEquals(title, component.getOriginalElement().getFirstComponentInSlot("title"));
    }

    @Test
    void shouldAddToCaption() {
      Component caption = new Card();
      component.addToCaption(caption);

      assertEquals(caption, component.getOriginalElement().getFirstComponentInSlot("caption"));
    }

    @Test
    void shouldAddToHeaderActions() {
      Component action = new Card();
      component.addToHeaderActions(action);

      assertEquals(action,
          component.getOriginalElement().getFirstComponentInSlot("header-actions"));
    }

    @Test
    void shouldAddToFooter() {
      Component footer = new Card();
      component.addToFooter(footer);

      assertEquals(footer, component.getOriginalElement().getFirstComponentInSlot("footer"));
    }

    @Test
    void shouldAddToBody() {
      Component body = new Card();
      component.addToBody(body);

      assertEquals(1, component.getComponentCount());
      assertTrue(component.hasComponent(body));
      assertNull(component.getOriginalElement().findComponentSlot(body));
    }

    @Test
    void shouldRemoveSlottedComponent() {
      Component footer = new Card();
      component.addToFooter(footer);
      component.remove(footer);

      assertNull(component.getOriginalElement().getFirstComponentInSlot("footer"));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddClickListener() {
      component.onClick(event -> {
      });

      List<EventListener<ElementClickEvent>> listeners =
          component.getEventListeners(ElementClickEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<ElementClickEvent>);
    }

    @Test
    void shouldRemoveClickListener() {
      ListenerRegistration<ElementClickEvent<Card>> registration = component.addClickListener(e -> {
      });

      assertEquals(1, component.getEventListeners(ElementClickEvent.class).size());

      registration.remove();

      assertEquals(0, component.getEventListeners(ElementClickEvent.class).size());
    }
  }
}
