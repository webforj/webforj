package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class PushMessageTest {


  @Test
  void shouldRequireTheTitle() {
    assertThrows(IllegalArgumentException.class, () -> PushMessage.create(" "));
    assertThrows(IllegalArgumentException.class, () -> PushMessage.create(null));
  }

  @Test
  void shouldKeepTitleAndBody() {
    PushMessage message = PushMessage.create("Order shipped").setBody("On its way").build();

    assertEquals("Order shipped", message.getTitle());
    assertEquals("On its way", message.getBody());
  }

  @Test
  void shouldNotShareTheActionListWithTheCaller() {
    List<PushAction> actions = new ArrayList<>();
    actions.add(new PushAction("track", "Track", null));
    PushMessage message = PushMessage.create("Title").setActions(actions).build();
    actions.clear();

    assertEquals(1, message.getActions().size());
    assertThrows(UnsupportedOperationException.class, () -> message.getActions().clear());
  }

  @Nested
  class Payload {

    @Test
    void shouldCarryOnlyTheTitleByDefault() {
      JsonObject json = payload(PushMessage.create("Title").build());

      assertEquals(1, json.size(), json.toString());
      assertEquals("Title", json.get("title").getAsString());
    }

    @Test
    void shouldCarryEveryVisualSetting() {
      PushMessage message = PushMessage.create("Title").setBody("Body").setIcon("icons://icon.png")
          .setTag("orders").setUrl("/orders/1").setSilent(true).build();

      JsonObject json = payload(message);

      assertEquals("Body", json.get("body").getAsString());
      assertEquals("icons://icon.png", json.get("icon").getAsString());
      assertEquals("orders", json.get("tag").getAsString());
      assertEquals("/orders/1", json.get("url").getAsString());
      assertTrue(json.get("silent").getAsBoolean());
    }

    @Test
    void shouldCarryActionsInOrder() {
      PushMessage message = PushMessage.create("Title")
          .setActions(List.of(new PushAction("track", "Track", "/orders/1/tracking"),
              new PushAction("dismiss", "Dismiss", null)))
          .build();

      JsonObject json = payload(message);

      assertEquals(2, message.getActions().size());
      assertEquals(2, json.getAsJsonArray("actions").size());
      JsonObject first = json.getAsJsonArray("actions").get(0).getAsJsonObject();
      assertEquals("track", first.get("action").getAsString());
      assertEquals("Track", first.get("title").getAsString());
      assertEquals("/orders/1/tracking", first.get("url").getAsString());
      assertFalse(json.getAsJsonArray("actions").get(1).getAsJsonObject().has("url"));
    }

    @Test
    void shouldTreatNullActionsAsNone() {
      PushMessage message = PushMessage.create("Title").setActions(null).build();

      assertTrue(message.getActions().isEmpty());
      assertFalse(payload(message).has("actions"));
    }

    @Test
    void shouldLeaveDeliverySettingsOutOfThePayload() {
      PushMessage message = PushMessage.create("Title").setTimeToLive(Duration.ofHours(1))
          .setUrgency(PushUrgency.HIGH).setTopic("orders").build();

      JsonObject json = payload(message);

      assertEquals(1, json.size(), json.toString());
      assertEquals(Duration.ofHours(1), message.getTimeToLive());
      assertEquals(PushUrgency.HIGH, message.getUrgency());
      assertEquals("orders", message.getTopic());
    }

    @Test
    void shouldReadBackEveryProperty() {
      PushMessage message =
          PushMessage.create("Title").setBody("b").setIcon("i").setTag("t").setUrl("u").build();

      assertEquals("b", message.getBody());
      assertEquals("i", message.getIcon());
      assertEquals("t", message.getTag());
      assertEquals("u", message.getUrl());
      assertFalse(message.isSilent());
      assertNull(message.getTimeToLive());
      assertNull(message.getUrgency());
      assertNull(message.getTopic());
    }
  }

  @Nested
  class IconProtocols {

    @Test
    void shouldRejectContextProtocol() {
      PushMessage.Builder builder = PushMessage.create("Title");
      IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
          () -> builder.setIcon("CONTEXT://images/icon.png"));

      assertTrue(e.getMessage().contains("4 KB"), e.getMessage());
    }

    @Test
    void shouldAcceptIconsAndWebServerProtocols() {
      assertEquals("icons://icon.png",
          PushMessage.create("Title").setIcon("icons://icon.png").build().getIcon());
      assertEquals("webserver://i.png",
          PushMessage.create("Title").setIcon("webserver://i.png").build().getIcon());
    }
  }

  private static JsonObject payload(PushMessage message) {
    return JsonParser.parseString(message.toPayload()).getAsJsonObject();
  }
}
