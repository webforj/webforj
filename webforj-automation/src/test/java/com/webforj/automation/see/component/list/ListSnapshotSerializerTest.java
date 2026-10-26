package com.webforj.automation.see.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.list.ComboBox;
import com.webforj.component.list.ListBox;
import com.webforj.component.list.MultipleSelectableList;
import org.junit.jupiter.api.Test;

class ListSnapshotSerializerTest {

  @Test
  void shouldDescribeTheItemsWithoutTheSelectionByDefault() {
    ListBox fruits = createFruits();

    assertEquals("""
        - ListBox [multiple]:
          - ListItem "Apple" [key=apple]
          - ListItem "Pear" [key=pear]
          - ListItem "Plum" [key=plum]
        """, Snapshot.render(fruits));
  }

  @Test
  void shouldFlagEverySelectedItemWithBrowserState() {
    ListBox fruits = createFruits();

    assertEquals("""
        - ListBox [multiple]:
          - ListItem "Apple" [key=apple]
          - ListItem "Pear" [key=pear] [selected]
          - ListItem "Plum" [key=plum] [selected]
        """, Snapshot.render(fruits, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveOutTheTextAndValueThatRepeatTheSelectedItemWithBrowserState() {
    ChoiceBox country = new ChoiceBox("Country");
    country.add("de", "Germany");
    country.add("fr", "France");
    country.selectKey("de");

    assertEquals("""
        - ChoiceBox [label=Country]:
          - ListItem "Germany" [key=de] [selected]
          - ListItem "France" [key=fr]
        """, Snapshot.render(country, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldKeepTheTypedTextThatIsNoItemWithBrowserState() {
    ComboBox customer = new ComboBox();
    customer.add("ada", "Ada");
    customer.add("linus", "Linus");
    customer.setText("Ad");

    assertEquals("""
        - ComboBox "Ad":
          - ListItem "Ada" [key=ada]
          - ListItem "Linus" [key=linus]
        """, Snapshot.render(customer, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldElideTheItemsBeyondTheWidthAndCountThem() {
    ListBox fruits = createFruits();
    fruits.add("fig", "Fig");

    assertEquals("""
        - ListBox [multiple]:
          - ListItem "Apple" [key=apple]
          - ListItem "Pear" [key=pear]
          - … 2 more
        """, Snapshot.render(fruits, new SnapshotOptions().setMaxWidth(2)));
  }

  @Test
  void shouldLeaveTheSingleSelectionListUnflagged() {
    ListBox fruits = new ListBox();
    fruits.add("apple", "Apple");

    assertEquals("""
        - ListBox:
          - ListItem "Apple" [key=apple]
        """, Snapshot.render(fruits));
  }

  private static ListBox createFruits() {
    ListBox fruits = new ListBox();
    fruits.setSelectionMode(MultipleSelectableList.SelectionMode.MULTIPLE);
    fruits.add("apple", "Apple");
    fruits.add("pear", "Pear");
    fruits.add("plum", "Plum");
    fruits.selectKey("pear", "plum");

    return fruits;
  }
}
