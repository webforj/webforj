package com.webforj.data;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.data.repository.CollectionRepository;
import com.webforj.data.repository.Repository;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PaginatorTest {
  private Paginator paginator;

  @BeforeEach
  void setUp() {
    paginator = new Paginator(100);
  }

  @Test
  void testConstructor() {
    assertEquals(100, paginator.getTotalItems());
    assertEquals(1, paginator.getCurrent());
    assertEquals(10, paginator.getSize());
    assertEquals(10, paginator.getMax());
  }

  @Test
  void testSetTotalItems() {
    paginator.setTotalItems(200);
    assertEquals(200, paginator.getTotalItems());
  }

  @Test
  void testSetTotalItemsNegative() {
    assertThrows(IllegalArgumentException.class, () -> paginator.setTotalItems(-1));
  }

  @Test
  void testSetCurrentPage() {
    paginator.setCurrent(5);
    assertEquals(5, paginator.getCurrent());
  }

  @Test
  void testSetCurrentPageNegative() {
    assertThrows(IllegalArgumentException.class, () -> paginator.setCurrent(0));
  }

  @Test
  void testSetPageSize() {
    paginator.setSize(20);
    assertEquals(20, paginator.getSize());
  }

  @Test
  void testSetPageSizeZero() {
    assertThrows(IllegalArgumentException.class, () -> paginator.setSize(0));
  }

  @Test
  void testSetMaxPages() {
    paginator.setMax(5);
    assertEquals(5, paginator.getMax());
  }

  @Test
  void testSetMaxPagesZero() {
    assertThrows(IllegalArgumentException.class, () -> paginator.setMax(0));
  }

  @Test
  void testPaginationCalculations() {
    paginator.setTotalItems(200);
    paginator.setCurrent(5);
    paginator.setSize(20);
    paginator.setMax(5);

    assertEquals(10, paginator.getTotalPages());
    assertEquals(3, paginator.getStartPage());
    assertEquals(7, paginator.getEndPage());
    assertEquals(80, paginator.getStartIndex());
    assertEquals(99, paginator.getEndIndex());

    assertArrayEquals(new int[] {3, 4, 5, 6, 7}, paginator.getPages());
  }

  @Test
  void testPaginationCalculationsNearEnd() {
    paginator.setTotalItems(200);
    paginator.setCurrent(9); // Set currentPage near the end
    paginator.setSize(20);
    paginator.setMax(5);

    assertEquals(10, paginator.getTotalPages());
    assertEquals(6, paginator.getStartPage()); // totalPages - maxPages + 1
    assertEquals(10, paginator.getEndPage()); // totalPages
    assertEquals(160, paginator.getStartIndex());
    assertEquals(179, paginator.getEndIndex());

    assertArrayEquals(new int[] {6, 7, 8, 9, 10}, paginator.getPages());
  }

  @Test
  void shouldListenToRepositoryChanges() {
    Repository<String> repo = new CollectionRepository<>(List.of("one", "two", "three"));
    paginator = new Paginator(repo);

    assertEquals(3, paginator.getTotalItems());

    repo.setFilter(s -> s.equals("one"));
    repo.commit();

    assertEquals(1, paginator.getTotalItems());
  }

  @Test
  void shouldUpdateRepository() {
    Repository<String> repo = new CollectionRepository<>(
        List.of("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"));
    paginator = new Paginator(repo, 2);

    assertEquals(10, paginator.getTotalItems());

    paginator.setCurrent(2);
    assertEquals(2, repo.getOffset());
    assertEquals(2, repo.getLimit());
    assertIterableEquals(List.of("three", "four"), repo.findAll().toList());
  }
}
