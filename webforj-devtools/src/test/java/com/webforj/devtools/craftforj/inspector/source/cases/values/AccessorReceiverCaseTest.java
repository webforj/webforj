package com.webforj.devtools.craftforj.inspector.source.cases.values;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.list.ComboBox;
import com.webforj.component.list.DwcList;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.cases.support.SourceWriteFixture;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class AccessorReceiverCaseTest {

  private static final String SOURCE = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.list.ComboBox;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/list-accessor-cases", outlet = MainLayout.class)
      @FrameTitle("List accessor cases")
      public class ListAccessorCasesView extends Composite<FlexLayout> {
        public ListAccessorCasesView() {
          ComboBox choice = new ComboBox("Page");
          choice.setPlaceholder("Choose a page");
          choice.getSearch().setFieldVisible(true).setPlaceholder("Filter pages");
          choice.insert("Sources", "Properties");
          choice.onSelect(event -> Toast.show(event.getSelectedItem().getText(), 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
          getBoundComponent().add(choice);
        }
      }
      """;
  private static final String EXPECTED = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.list.ComboBox;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/list-accessor-cases", outlet = MainLayout.class)
      @FrameTitle("List accessor cases")
      public class ListAccessorCasesView extends Composite<FlexLayout> {
        public ListAccessorCasesView() {
          ComboBox choice = new ComboBox("Page");
          choice.setPlaceholder("Choose destination");
          choice.getSearch().setFieldVisible(true).setPlaceholder("Filter pages");
          choice.insert("Sources", "Properties");
          choice.onSelect(event -> Toast.show(event.getSelectedItem().getText(), 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
          getBoundComponent().add(choice);
        }
      }
      """;
  private static final String RESET = """
      package com.devtoolsapplayoutspring.views;

      import com.webforj.component.Composite;
      import com.webforj.component.Theme;
      import com.webforj.component.list.ComboBox;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.toast.Toast;
      import com.webforj.router.annotation.FrameTitle;
      import com.webforj.router.annotation.Route;

      @Route(value = "/list-accessor-cases", outlet = MainLayout.class)
      @FrameTitle("List accessor cases")
      public class ListAccessorCasesView extends Composite<FlexLayout> {
        public ListAccessorCasesView() {
          ComboBox choice = new ComboBox("Page");
          choice.setPlaceholder("Choose destination");
          choice.getSearch().setFieldVisible(true);
          choice.insert("Sources", "Properties");
          choice.onSelect(event -> Toast.show(event.getSelectedItem().getText(), 3000, Theme.INFO,
              Toast.Placement.BOTTOM_RIGHT));
          getBoundComponent().add(choice);
        }
      }
      """;

  @TempDir
  Path temporaryDirectory;

  @SuppressWarnings("unchecked")
  @Test
  @DisplayName("Direct placeholder edits and nested search resets keep their own receivers")
  void preserveDistinctReceivers() throws IOException {
    try (SourceWriteFixture fixture = new SourceWriteFixture(temporaryDirectory)) {
      Path file =
          fixture.addSource("com.devtoolsapplayoutspring.views.ListAccessorCasesView", SOURCE);
      ComboBox choice = fixture.addComponent("choice", ComboBox.class,
          List.of(new SourcePoint("com.devtoolsapplayoutspring.views.ListAccessorCasesView",
              "ListAccessorCasesView.java", 15)));
      DwcList<ComboBox, Object>.Search search = mock(DwcList.Search.class);
      when(choice.getSearch()).thenReturn(search);
      SourceCodeModifier modifier = fixture.getModifier();
      ChangeRequest edit = fixture.createChange("choice", "HasPlaceholder", "Choose destination");

      List<FilePatch> patches = modifier.previewPatches(List.of(edit));
      assertEquals(List.of(EXPECTED), patches.stream().map(FilePatch::getPatched).toList());
      assertEquals(SOURCE, Files.readString(file));
      modifier.apply(List.of(edit));
      assertEquals(EXPECTED, Files.readString(file));

      ChangeRequest reset = fixture.createChange("choice", "ListSearchPlaceholder", "");
      List<FilePatch> resetPatches = modifier.previewPatches(List.of(reset));
      assertEquals(List.of(RESET), resetPatches.stream().map(FilePatch::getPatched).toList());
      assertEquals(EXPECTED, Files.readString(file));
      modifier.apply(List.of(reset));
      assertEquals(RESET, Files.readString(file));
    }
  }
}
