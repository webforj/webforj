package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ApplyChangesActionTest {

  private SourceCodeModifier modifier;
  private ApplyChangesAction action;

  @BeforeEach
  void setUp() {
    modifier = mock(SourceCodeModifier.class);
    action = new ApplyChangesAction(modifier);
  }

  @Test
  void shouldReturnCorrectActionName() {
    assertEquals("inspector.applyChanges", action.getAction());
  }

  @Nested
  class Handle {

    @Test
    void shouldReturnEmptyResultWhenNoChanges() {
      JsonObject params = new JsonObject();
      when(modifier.apply(anyList())).thenReturn(List.of());

      List<ChangeResult> result = action.handle(params);

      assertNotNull(result);
      assertTrue(result.isEmpty());
    }

    @Test
    void shouldParseAndPassChangesToModifier() {
      JsonObject change = new JsonObject();
      change.addProperty("componentId", "comp-1");
      change.addProperty("featureType", "HasText");
      change.addProperty("propertyName", "Text");
      change.addProperty("value", "Hello");
      JsonArray changes = new JsonArray();
      changes.add(change);

      JsonObject params = new JsonObject();
      params.add("changes", changes);
      when(modifier.apply(anyList())).thenReturn(List.of());

      action.handle(params);

      verify(modifier).apply(anyList());
    }

    @Test
    void shouldCallPreviewWhenDryRunTrue() {
      JsonObject params = new JsonObject();
      params.addProperty("dryRun", true);
      params.add("changes", new JsonArray());
      when(modifier.preview(anyList())).thenReturn(List.of());

      action.handle(params);

      verify(modifier).preview(anyList());
    }

    @Test
    void shouldCallApplyWhenDryRunFalse() {
      JsonObject params = new JsonObject();
      params.addProperty("dryRun", false);
      params.add("changes", new JsonArray());
      when(modifier.apply(anyList())).thenReturn(List.of());

      action.handle(params);

      verify(modifier).apply(anyList());
    }

    @Test
    void shouldCallApplyWhenDryRunMissing() {
      JsonObject params = new JsonObject();
      params.add("changes", new JsonArray());
      when(modifier.apply(anyList())).thenReturn(List.of());

      action.handle(params);

      verify(modifier).apply(anyList());
    }
  }
}
