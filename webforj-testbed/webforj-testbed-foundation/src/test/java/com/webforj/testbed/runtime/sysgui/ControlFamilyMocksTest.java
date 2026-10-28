package com.webforj.testbed.runtime.sysgui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjSelectionChangeEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjEditBoxSpinner;
import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjInputDSpinner;
import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.basis.bbj.proxies.sysgui.BBjInputT;
import com.basis.bbj.proxies.sysgui.BBjInputTSpinner;
import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjListEdit;
import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.basis.util.common.BasisNumber;
import com.webforj.testbed.runtime.RuntimeMock;
import com.webforj.testbed.runtime.event.BBjWebEventOptionsMock;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class ControlFamilyMocksTest {

  @Test
  void shouldTrackTreeNodesSelectionAndExpansion() throws BBjException {
    BBjTreeMock mock = new BBjTreeMock(1);
    BBjTree tree = mock.getProxy(BBjTree.class);

    tree.setRootVisible(false);
    tree.setRoot(0, "root");
    tree.insertNode(1, 0, "fruits", 0);
    tree.insertNode(2, 1, "apple", 0);
    tree.insertNode(3, 1, "pear", 1);

    assertEquals(List.of(0, 1, 2, 3), mock.getNodeIds());

    tree.setNodeText(2, "green apple");
    tree.setToolTipText(2, "a fruit");

    tree.selectNode(2);

    assertEquals(2, tree.getSelectedNode());
    assertEquals(List.of(BasisNumber.valueOf(2)), tree.getSelectedNodes());

    tree.selectChildren(1);
    tree.deselectNode(2);
    tree.deselectChildren(1);
    tree.deselectAll();

    assertEquals(-1, tree.getSelectedNode());
    assertTrue(tree.getSelectedNodes().isEmpty());

    tree.expandNode(1);

    assertTrue(tree.isNodeExpanded(1));

    tree.collapseNode(1);

    assertFalse(tree.isNodeExpanded(1));

    tree.removeDescendants(1);
    tree.removeNode(1);

    assertEquals(List.of(0), mock.getNodeIds());
  }

  @Test
  void shouldRejectNegativeTabIndexes() throws BBjException {
    BBjTabCtrl tabCtrl = new BBjTabCtrlMock(1).getProxy(BBjTabCtrl.class);
    tabCtrl.addTab("First", -1);

    assertThrows(BBjException.class, () -> tabCtrl.getTitleAt(-1));
    assertEquals("First", tabCtrl.getTitleAt(0));
  }

  @Test
  void shouldKeepTabStateAndSelectionInsideRange() throws BBjException {
    BBjTabCtrlMock mock = new BBjTabCtrlMock(1);
    BBjTabCtrl tabCtrl = mock.getProxy(BBjTabCtrl.class);

    assertEquals(-1, tabCtrl.getSelectedIndex());

    tabCtrl.addTab("First", -1);
    tabCtrl.insertTab(1, "Third", -1);
    tabCtrl.insertTab(1, "Second", -1);

    assertEquals(List.of("First", "Second", "Third"), mock.getTitles());
    assertEquals(0, tabCtrl.getSelectedIndex());

    tabCtrl.setSelectedIndex(2, false);

    assertEquals(2, tabCtrl.getSelectedIndex());

    tabCtrl.setTitleAt(2, "Last");
    assertThrows(BBjException.class, () -> tabCtrl.setTitleAt(99, "missing"));
    tabCtrl.setEnabledAt(0, false);
    tabCtrl.setCloseableAt(0, true);
    tabCtrl.setToolTipTextAt(0, "tip");

    assertEquals("Last", mock.getTitles().get(2));
    assertEquals("Last", tabCtrl.getTitleAt(2));
    assertEquals(3, tabCtrl.getNumTabs());
    assertFalse(tabCtrl.isEnabledAt(0));
    assertTrue(tabCtrl.isCloseableAt(0));

    tabCtrl.removeTab(2);

    assertEquals(1, tabCtrl.getSelectedIndex());

    assertThrows(BBjException.class, () -> tabCtrl.removeTab(99));
    tabCtrl.removeTab(0);
    tabCtrl.removeTab(0);

    assertEquals(-1, tabCtrl.getSelectedIndex());
  }

  @Test
  void shouldKeepParagraphAndSelectionState() throws BBjException {
    BBjCEditMock mock = new BBjCEditMock(1);
    BBjCEdit textArea = mock.getProxy(BBjCEdit.class);

    textArea.addParagraph(-1, "first");
    textArea.addParagraph(0, "zero");
    textArea.addParagraph(99, "last");
    textArea.appendToParagraph(0, " paragraph");
    textArea.appendToParagraph(99, "ignored");

    assertEquals(List.of("zero paragraph", "first", "last"), mock.getParagraphs());

    assertThrows(BBjException.class, () -> textArea.removeParagraph(99));
    assertThrows(BBjException.class, () -> textArea.setMaxLength(-1));
    textArea.removeParagraph(0);

    assertEquals(List.of("first", "last"), mock.getParagraphs());

    textArea.highlight(0, 2, 1, 3);

    assertEquals(List.of(0, 2, 1, 3), textArea.getSelection());
  }

  @Test
  void shouldTrackRadioGroupMembership() throws BBjException {
    BBjRadioGroupMock mock = new BBjRadioGroupMock();
    BBjRadioGroup group = mock.getProxy(BBjRadioGroup.class);
    BBjRadioButton yes = new BBjRadioButtonMock(1).getProxy(BBjRadioButton.class);
    BBjRadioButton no = new BBjRadioButtonMock(2).getProxy(BBjRadioButton.class);

    group.add(yes);
    group.add(no);

    assertEquals(List.of(yes, no), mock.getMembers());
    assertNull(group.getSelected());

    mock.select(yes);

    assertEquals(yes, group.getSelected());

    List<String> events = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjSelectionChangeEvent event) {
        events.add("changed");
      }
    };

    group.setCallback(SysGuiEventConstants.ON_SELECTION_CHANGE, handler, "handleEvent");
    mock.fireEvent(SysGuiEventConstants.ON_SELECTION_CHANGE,
        RuntimeMock.createEvent(BBjSelectionChangeEvent.class, Map.of()));

    assertEquals(List.of("changed"), events);

    group.clearCallback(SysGuiEventConstants.ON_SELECTION_CHANGE);

    assertFalse(mock.fireEvent(SysGuiEventConstants.ON_SELECTION_CHANGE,
        RuntimeMock.createEvent(BBjSelectionChangeEvent.class, Map.of())));

    group.remove(yes);

    assertNull(group.getSelected());
    assertEquals(List.of(no), mock.getMembers());
  }

  @Test
  void shouldSpinThroughOptionLists() throws BBjException {
    BBjEditBoxSpinner spinner = new BBjEditBoxSpinnerMock(1).getProxy(BBjEditBoxSpinner.class);

    spinner.spin(true);

    assertEquals(0, spinner.getListIndex());

    spinner.setSpinList(new BBjVector(List.of("a", "b", "c")));
    spinner.setListIndex(1);
    spinner.spin(true);

    assertEquals(2, spinner.getListIndex());

    spinner.spin(true);

    assertEquals(2, spinner.getListIndex());

    spinner.spin(false);
    spinner.spin(false);
    spinner.spin(false);

    assertEquals(0, spinner.getListIndex());
  }

  @Test
  void shouldRejectListIndexesOutsideTheSpinList() throws BBjException {
    BBjEditBoxSpinner spinner = new BBjEditBoxSpinnerMock(1).getProxy(BBjEditBoxSpinner.class);
    spinner.setSpinList(new BBjVector(List.of("a", "b")));

    assertThrows(BBjException.class, () -> spinner.setListIndex(-1));
    assertThrows(BBjException.class, () -> spinner.setListIndex(2));
    assertEquals(0, spinner.getListIndex());
  }

  @Test
  void shouldKeepMaskedInputState() throws BBjException {
    BBjInputN numberField = new BBjInputNMock(1).getProxy(BBjInputN.class);

    numberField.setMask("###");
    numberField.setRestore("0");
    numberField.setInsertMode(true);
    numberField.setCaretPosition(2);
    numberField.setCommaCharacter(".");
    numberField.setDotCharacter(",");
    numberField.setNegateable(false);

    assertEquals("###", numberField.getMask());
    assertEquals("0", numberField.getRestore());
    assertTrue(numberField.getInsertMode());
    assertEquals(2, numberField.getCaretPosition());
    assertEquals(".", numberField.getCommaCharacter());
    assertEquals(",", numberField.getDotCharacter());
    assertFalse(numberField.getNegateable());

    BBjInputD dateField = new BBjInputDMock(2).getProxy(BBjInputD.class);

    dateField.setValue(BasisNumber.valueOf(2460000));

    assertEquals(BasisNumber.valueOf(2460000), dateField.getValue());
  }

  @Test
  void shouldKeepEditBoxSelectionState() throws BBjException {
    BBjEditBoxMock mock = new BBjEditBoxMock(1);
    BBjEditBox editBox = mock.getProxy(BBjEditBox.class);

    assertEquals("", editBox.getSelectedText());

    editBox.setText("hello world");
    editBox.select(6, 11);

    assertEquals(List.of(0, 6, 0, 11), editBox.getSelection());
    assertEquals("world", editBox.getSelectedText());

    editBox.select(5, 2);

    assertEquals("", editBox.getSelectedText());
  }

  @Test
  void shouldToggleAndWrapSimpleControls() throws BBjException {
    BBjStaticText label = new BBjStaticTextMock(1).getProxy(BBjStaticText.class);

    label.setLineWrap(false);

    assertFalse(label.getLineWrap());

    BBjButton button = new BBjButtonMock(2).getProxy(BBjButton.class);

    button.setDisableOnClick(true);

    assertTrue(button.getDisableOnClick());
  }


  @Test
  void shouldStyleTopLevelWindowAreas() throws BBjException {
    BBjTopLevelWindow window = new BBjTopLevelWindowMock(1).getProxy(BBjTopLevelWindow.class);

    window.setOuterStyle("color", "red");
    window.setPanelStyle("color", "blue");
    window.addOuterClass("outer");
    window.addPanelClass("panel");
    window.removeOuterClass("outer");
    window.removePanelClass("panel");

    assertEquals("red", window.getOuterStyle("color"));
    assertEquals("blue", window.getPanelStyle("color"));
    assertEquals("blue", window.getComputedPanelStyle("color"));
    assertNull(window.getOuterStyle("missing"));
  }

  @Test
  void shouldConfigureEventOptions() throws BBjException {
    BBjWebEventOptionsMock mock = new BBjWebEventOptionsMock();
    BBjWebEventOptions options = mock.getOptions();

    options.setCode("code").setFilter("filter").addItem("x", "event.x").setDebounce(250, true,
        false);

    assertEquals("code", options.getCode());
    assertEquals("filter", options.getFilter());
    assertEquals(Map.of("x", "event.x"), options.getItemMap());
    assertTrue(options.getDebounce());
    assertEquals(250, options.getDelay());
    assertTrue(options.getLeading());
    assertFalse(options.getTrailing());

    options.setThrottle(100).setImmediate();

    assertTrue(options.getThrottle());
    assertTrue(options.getImmediate());
    assertEquals(100, options.getDelay());
  }

  @Test
  void shouldFireControlEventsOnlyWhenRegistered() {
    BBjControlMock mock = new BBjControlMock(1);

    assertFalse(mock.fireEvent(SysGuiEventConstants.ON_BUTTON_PUSH,
        RuntimeMock.createEvent(BBjEvent.class, Map.of())));
    assertFalse(mock.hasCallback(SysGuiEventConstants.ON_BUTTON_PUSH));
  }

  @Test
  void shouldFormatTheDateTextFromTheValueAndParseTheValueFromTheText() throws BBjException {
    BBjInputD dateField = new BBjInputDMock(1).getProxy(BBjInputD.class);

    assertEquals(BasisNumber.valueOf(-1), dateField.getValue());
    assertEquals("", dateField.getText());

    dateField.setValue(BasisNumber.valueOf(2460325));

    assertEquals("01/15/2024", dateField.getText());

    dateField.setText("03/02/2020");

    assertEquals(BasisNumber.valueOf(2458911), dateField.getValue());

    dateField.setText("");

    assertEquals(BasisNumber.valueOf(-1), dateField.getValue());

    dateField.setValue(BasisNumber.valueOf(-1));

    assertEquals("", dateField.getText());
  }

  @Test
  void shouldFormatTheTimeTextFromTheValueAndParseTheValueFromTheText() throws BBjException {
    BBjInputT timeField = new BBjInputTMock(1).getProxy(BBjInputT.class);

    assertEquals(BasisNumber.valueOf(-1), timeField.getValue());

    timeField.setValue(BasisNumber.valueOf(10.5));

    assertEquals("10:30 AM", timeField.getText());

    timeField.setText("2:15 PM");

    assertEquals(BasisNumber.valueOf(14.25), timeField.getValue());

    timeField.setText("");

    assertEquals(BasisNumber.valueOf(-1), timeField.getValue());

    timeField.setValue(BasisNumber.valueOf(-1));

    assertEquals("", timeField.getText());
  }

  @Test
  void shouldStepTheDateSpinnerByTheSpinField() throws BBjException {
    BBjInputDSpinner spinner = new BBjInputDSpinnerMock(1).getProxy(BBjInputDSpinner.class);

    spinner.spin(true);

    assertEquals("", spinner.getText());

    spinner.setText("01/15/2024");
    spinner.spin(true);

    assertEquals("01/16/2024", spinner.getText());

    spinner.setSpinField(BBjInputDSpinner.MONTH);
    spinner.spin(false);

    assertEquals("12/16/2023", spinner.getText());

    spinner.setSpinField(BBjInputDSpinner.YEAR);
    spinner.spin(true);

    assertEquals("12/16/2024", spinner.getText());

    spinner.setSpinField(BBjInputDSpinner.WEEK);
    spinner.spin(true);

    assertEquals("12/23/2024", spinner.getText());
    assertEquals(BBjInputDSpinner.WEEK, spinner.getSpinField());
  }

  @Test
  void shouldStepTheTimeSpinnerByTheSpinField() throws BBjException {
    BBjInputTSpinner spinner = new BBjInputTSpinnerMock(1).getProxy(BBjInputTSpinner.class);

    spinner.spin(true);

    assertEquals("", spinner.getText());

    spinner.setText("10:30 AM");
    spinner.spin(true);

    assertEquals("11:30 AM", spinner.getText());

    spinner.setSpinField(BBjInputTSpinner.MINUTE);
    spinner.spin(false);

    assertEquals("11:29 AM", spinner.getText());

    spinner.setSpinField(BBjInputTSpinner.SECOND);
    spinner.spin(true);

    assertEquals(11.0 + 29.0 / 60 + 1.0 / 3600, spinner.getValue().doubleValue(), 1e-9);
  }

  @Test
  void shouldShareNumericAndTextValuesInNumberInputs() throws BBjException {
    BBjInputN input = new BBjInputNMock(1).getProxy(BBjInputN.class);

    input.setValue(BasisNumber.valueOf(12.5));
    assertEquals("12.5", input.getText());
    input.setText(" 7.25 ");
    assertEquals(7.25, input.getValue().doubleValue());
    input.setText("");
    assertEquals(0d, input.getValue().doubleValue());
    input.setText(".");
    assertEquals(0d, input.getValue().doubleValue());
    input.setText("invalid");
    assertThrows(BBjException.class, input::getValue);
  }

  @Test
  void shouldReadTheNumericValueFromTheSpinnerText() throws BBjException {
    BBjInputNSpinner spinner = new BBjInputNSpinnerMock(1).getProxy(BBjInputNSpinner.class);
    spinner.setValue(BasisNumber.valueOf(12));
    spinner.setText("7.25");

    assertEquals(7.25, spinner.getValue().doubleValue());
  }

  @Test
  void shouldApplyNativeSpinnerSettingsToTheClientState() throws BBjException {
    BBjInputNSpinner spinner = new BBjInputNSpinnerMock(1).getProxy(BBjInputNSpinner.class);
    spinner.setValue(BasisNumber.valueOf(10));
    spinner.setStepSize(BasisNumber.valueOf(-4));
    spinner.setMinimum(BasisNumber.valueOf(9));
    spinner.setMaximum(BasisNumber.valueOf(12));

    spinner.spin(true);
    assertEquals(12d, spinner.getValue().doubleValue());
    spinner.spin(false);
    assertEquals(9d, spinner.getValue().doubleValue());
    assertEquals(-4d, spinner.getStepSize().doubleValue());
    assertEquals(9d, spinner.getMinimum().doubleValue());
    assertEquals(12d, spinner.getMaximum().doubleValue());
  }

  @Test
  void shouldKeepNativeSpinnerSettingsIndependentOfDirectClientProperties() throws BBjException {
    BBjInputNSpinner spinner = new BBjInputNSpinnerMock(1).getProxy(BBjInputNSpinner.class);
    assertEquals(1d, spinner.getStepSize().doubleValue());
    assertNull(spinner.getMinimum());
    assertNull(spinner.getMaximum());
    spinner.setStepSize(BasisNumber.valueOf(2));
    spinner.setMinimum(BasisNumber.valueOf(3));
    spinner.setMaximum(BasisNumber.valueOf(20));

    spinner.setProperty("step", 5d);
    spinner.setProperty("min", 7d);
    spinner.setProperty("max", 15d);
    spinner.setValue(BasisNumber.valueOf(10));
    spinner.spin(true);
    assertEquals(15d, spinner.getValue().doubleValue());
    assertEquals(2d, spinner.getStepSize().doubleValue());
    assertEquals(3d, spinner.getMinimum().doubleValue());
    assertEquals(20d, spinner.getMaximum().doubleValue());

    spinner.setStepSize(null);
    spinner.setMinimum(null);
    spinner.setMaximum(null);
    assertEquals(1d, spinner.getStepSize().doubleValue());
    assertNull(spinner.getMinimum());
    assertNull(spinner.getMaximum());
    assertEquals(-Double.MAX_VALUE, spinner.getProperty("min"));
    assertEquals(Double.MAX_VALUE, spinner.getProperty("max"));
    spinner.spin(true);
    assertEquals(16d, spinner.getValue().doubleValue());
  }

  @Test
  void shouldUseDecimalPrecisionOnlyForOneMaskSeparator() throws BBjException {
    BBjInputNSpinner spinner = new BBjInputNSpinnerMock(1).getProxy(BBjInputNSpinner.class);
    spinner.setText("0");
    spinner.setProperty("step", 0.15);
    spinner.setMask("#.##.##");

    spinner.spin(true);

    assertEquals(0d, spinner.getValue().doubleValue());
  }

  @Test
  void shouldStepTheNumberSpinnerByOne() throws BBjException {
    BBjInputNSpinner spinner = new BBjInputNSpinnerMock(1).getProxy(BBjInputNSpinner.class);

    spinner.spin(true);

    assertEquals(1.0, spinner.getValue().doubleValue());

    spinner.spin(false);
    spinner.spin(false);

    assertEquals(-1.0, spinner.getValue().doubleValue());
  }

  @Test
  void shouldSpinFromValuesWrittenThroughTheNativeNumberSetter() throws BBjException {
    BBjInputNSpinner spinner = new BBjInputNSpinnerMock(1).getProxy(BBjInputNSpinner.class);
    spinner.setValue(BasisNumber.valueOf(10));

    spinner.spin(true);

    assertEquals(11d, spinner.getValue().doubleValue());
    assertEquals("11", spinner.getText());
    spinner.setValue(BasisNumber.valueOf(20));
    spinner.spin(false);
    assertEquals(19d, spinner.getValue().doubleValue());
    spinner.setValue(BasisNumber.valueOf(10_000_000));
    spinner.spin(true);
    assertEquals("10000001", spinner.getText());
  }

  @Test
  void shouldMoveTheSpinListToTypedTextAndGuardTheListIndex() throws BBjException {
    BBjEditBoxSpinner spinner = new BBjEditBoxSpinnerMock(1).getProxy(BBjEditBoxSpinner.class);

    assertThrows(BBjException.class, () -> spinner.setSpinList(null));
    assertThrows(BBjException.class, () -> spinner.setListIndex(0));

    spinner.setSpinList(new BBjVector(List.of("a", "b", "c")));
    spinner.setListIndex(2);

    assertEquals("c", spinner.getText());
    assertEquals(List.of("a", "b", "c"), spinner.getSpinList());

    spinner.setText("b");

    assertEquals(1, spinner.getListIndex());

    spinner.setText("zzz");

    assertEquals(1, spinner.getListIndex());
    assertThrows(BBjException.class, () -> spinner.setListIndex(3));
  }

  @Test
  void shouldSelectTheItemMatchingTheTypedTextIgnoringCase() throws BBjException {
    BBjListEdit listEdit = new BBjListEditMock(1).getProxy(BBjListEdit.class);
    listEdit.addItem("One");
    listEdit.addItem("Two");

    listEdit.setText("two");

    assertEquals(1, listEdit.getSelectedIndex());
    assertEquals("two", listEdit.getText());

    listEdit.setText("three");

    assertEquals(-1, listEdit.getSelectedIndex());
    assertEquals("three", listEdit.getText());
  }

  @Test
  void shouldReportTheSelectedItemAsTheListButtonText() throws BBjException {
    BBjListButton listButton = new BBjListButtonMock(1).getProxy(BBjListButton.class);
    listButton.addItem("One");

    assertEquals("", listButton.getText());

    listButton.selectIndex(0);

    assertEquals("One", listButton.getText());
  }

  @Test
  void shouldReplaceTheTreeSelectionInSingleModeAndExpandRecursively() throws BBjException {
    BBjTreeMock mock = new BBjTreeMock(1);
    BBjTree tree = mock.getProxy(BBjTree.class);
    tree.setRoot(0, "root");
    tree.insertNode(1, 0, "fruits", 0);
    tree.insertNode(2, 1, "apple", 0);
    tree.insertNode(3, 1, "pear", 1);

    assertEquals(0, mock.getRootId());
    assertTrue(tree.isNodeExpanded(0));
    assertThrows(BBjException.class, () -> tree.insertNode(2, 0, "again", 0));
    assertThrows(BBjException.class, () -> tree.selectNode(99));

    tree.selectNode(2);
    tree.selectNode(3);

    assertEquals(3, tree.getSelectedNode());
    assertEquals(List.of(BasisNumber.valueOf(2), BasisNumber.valueOf(3)), tree.getSelectedNodes());

    tree.setSelectionMode(SysGuiProxyConstants.SINGLE_TREE_SELECTION.intValue());
    tree.selectNode(2);

    assertEquals(SysGuiProxyConstants.SINGLE_TREE_SELECTION.intValue(), tree.getSelectionMode());
    assertEquals(List.of(BasisNumber.valueOf(2)), tree.getSelectedNodes());
    assertTrue(tree.isNodeSelected(2));
    assertFalse(tree.isNodeSelected(3));

    tree.expandTreeFromNode(1);

    assertTrue(tree.isNodeExpanded(1));
    assertTrue(tree.isNodeExpanded(2));

    tree.collapseTreeFromNode(0);

    assertFalse(tree.isNodeExpanded(0));
    assertFalse(tree.isNodeExpanded(2));

    tree.setRoot(7, "other");

    assertEquals(7, tree.getRoot());
    assertTrue(tree.getSelectedNodes().isEmpty());
    assertThrows(BBjException.class, () -> tree.getNodeText(1));
  }

  @Test
  void shouldKeepRangeStateOfSlidersAndProgressBars() throws BBjException {
    BBjSlider slider = new BBjSliderMock(1).getProxy(BBjSlider.class);
    slider.setMinimum(10);
    slider.setMaximum(20);
    slider.setValue(15);

    assertEquals(10, slider.getMinimum());
    assertEquals(20, slider.getMaximum());
    assertEquals(15, slider.getValue());

    BBjProgressBar progressBar = new BBjProgressBarMock(2).getProxy(BBjProgressBar.class);
    progressBar.setMinimum(1);
    progressBar.setMaximum(5);
    progressBar.setValue(3);
    progressBar.setOrientation(1);

    assertEquals(1, progressBar.getMinimum());
    assertEquals(5, progressBar.getMaximum());
    assertEquals(3, progressBar.getValue());
    assertThrows(BBjException.class, () -> progressBar.setOrientation(2));
  }

  @Test
  void shouldMoveTheGroupSelectionWithTheMemberChecks() throws BBjException {
    BBjWindow window = new BBjWindowMock(1).getProxy(BBjWindow.class);
    BBjRadioGroup group = window.addRadioGroup();
    BBjRadioButton yes = window.addRadioButton(100, "Yes");
    BBjRadioButton no = window.addRadioButton(101, "No");
    group.add(yes);
    group.add(no);
    BBjRadioButton maybe = window.addRadioButton(102, "Maybe");

    assertThrows(BBjException.class, () -> group.add(yes));
    assertThrows(BBjException.class, () -> group.remove(maybe));

    yes.setSelected(true);

    assertEquals(yes, group.getSelected());

    no.setSelected(true);

    assertEquals(no, group.getSelected());
    assertFalse(yes.isSelected());

    no.setSelected(false);

    assertNull(group.getSelected());

    maybe.setSelected(true);
    group.add(maybe);

    assertEquals(maybe, group.getSelected());
  }
}
