package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.FilePatch;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class PreviewPatchActionTest {

  private SourceCodeModifier modifier;
  private PreviewPatchAction action;

  @BeforeEach
  void setUp() {
    modifier = mock(SourceCodeModifier.class);
    action = new PreviewPatchAction(modifier);
  }

  @Test
  void shouldReturnCorrectActionName() {
    assertEquals("inspector.previewPatch", action.getAction());
  }

  @Nested
  class Handle {

    @Test
    void shouldReturnEmptyResultWhenNoChanges() {
      List<FilePatch> result = action.handle(new JsonObject());

      assertTrue(result.isEmpty());
      verify(modifier, never()).previewPatches(anyList());
    }

    @Test
    void shouldParseAndPassChangesToModifier() {
      JsonObject change = new JsonObject();
      change.addProperty("componentId", "comp-1");
      JsonArray changes = new JsonArray();
      changes.add(change);

      JsonObject params = new JsonObject();
      params.add("changes", changes);
      when(modifier.previewPatches(anyList())).thenReturn(List.of());

      action.handle(params);

      ArgumentCaptor<List<ChangeRequest>> captor = ArgumentCaptor.forClass(List.class);
      verify(modifier).previewPatches(captor.capture());
      assertEquals(1, captor.getValue().size());
      assertEquals("comp-1", captor.getValue().get(0).getComponentId());
    }

    @Test
    void shouldReturnPatchesFromModifier() {
      JsonObject params = new JsonObject();
      params.add("changes", new JsonArray());
      when(modifier.previewPatches(anyList()))
          .thenReturn(List.of(new FilePatch("/View.java", "before", "after")));

      List<FilePatch> result = action.handle(params);

      assertEquals(1, result.size());
      assertEquals("/View.java", result.get(0).getFile());
      assertEquals("before", result.get(0).getOriginal());
      assertEquals("after", result.get(0).getPatched());
    }
  }
}
