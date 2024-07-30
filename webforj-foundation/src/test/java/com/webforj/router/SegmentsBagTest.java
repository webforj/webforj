package com.webforj.router;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

class SegmentsBagTest {

  @Test
  void shouldConstructEmptySegmentsBag() {
    SegmentsBag bag = new SegmentsBag();
    assertEquals(0, bag.size());
  }

  @Test
  void shouldConstructSegmentsBagFromPathString() {
    SegmentsBag bag = SegmentsBag.of("segment1/segment2/segment3");
    assertEquals(3, bag.size());
    assertEquals("segment1", bag.get(0).orElse(null));
    assertEquals("segment2", bag.get(1).orElse(null));
    assertEquals("segment3", bag.get(2).orElse(null));
  }

  @Test
  void shouldConstructSegmentsBagFromList() {
    List<String> segments = Arrays.asList("segment1", "segment2", "segment3");
    SegmentsBag bag = SegmentsBag.of(segments);
    assertEquals(3, bag.size());
    assertEquals("segment1", bag.get(0).orElse(null));
    assertEquals("segment2", bag.get(1).orElse(null));
    assertEquals("segment3", bag.get(2).orElse(null));
  }

  @Test
  void shouldAddNewSegment() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    assertEquals(1, bag.size());
    assertEquals("segment1", bag.get(0).orElse(null));
  }

  @Test
  void shouldAddNewSegmentAtIndex() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    bag.add(0, "segment0");
    assertEquals(2, bag.size());
    assertEquals("segment0", bag.get(0).orElse(null));
    assertEquals("segment1", bag.get(1).orElse(null));
  }

  @Test
  void shouldRemoveSegmentByValue() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    bag.remove("segment1");
    assertEquals(0, bag.size());
  }

  @Test
  void shouldRemoveSegmentByIndex() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    bag.add("segment2");
    bag.remove(0);
    assertEquals(1, bag.size());
    assertEquals("segment2", bag.get(0).orElse(null));
  }

  @Test
  void shouldCheckIfSegmentExists() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    assertTrue(bag.contains("segment1"));
    assertFalse(bag.contains("segment2"));
  }

  @Test
  void shouldReturnAllSegments() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    bag.add("segment2");
    List<String> allSegments = bag.all();
    assertEquals(2, allSegments.size());
    assertEquals("segment1", allSegments.get(0));
    assertEquals("segment2", allSegments.get(1));
  }

  @Test
  void shouldReturnSegmentWithDefaultValue() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    assertEquals("segment1", bag.get(0, "default"));
    assertEquals("default", bag.get(1, "default"));
  }

  @Test
  void shouldReturnPathString() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    bag.add("segment2");
    assertEquals("segment1/segment2", bag.getPath());
  }

  @Test
  void shouldIterateOverSegments() {
    SegmentsBag bag = new SegmentsBag();
    bag.add("segment1");
    bag.add("segment2");
    int count = 0;
    for (String segment : bag) {
      count++;
    }
    assertEquals(2, count);
  }
}
