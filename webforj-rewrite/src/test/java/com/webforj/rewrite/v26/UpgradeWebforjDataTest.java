package com.webforj.rewrite.v26;

import static org.openrewrite.java.Assertions.java;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RecipeSpec;
import org.openrewrite.test.RewriteTest;

@SuppressWarnings("java:S2699")
class UpgradeWebforjDataTest implements RewriteTest {

  @Override
  public void defaults(RecipeSpec spec) {
    spec.recipeFromResources("com.webforj.rewrite.v26.UpgradeWebforjData");
  }

  /**
   * A view that filters a music list as the user types in a search field. This is the most common
   * repository usage.
   */
  @Test
  void renamesSetFilterToSetBaseFilter() {
    rewriteRun(java("""
        import com.webforj.data.repository.CollectionRepository;
        import java.util.List;

        class MusicView {
          private String searchTerm = "";
          private CollectionRepository<String> repository = new CollectionRepository<>(List.of());

          void onSearch(String term) {
            this.searchTerm = term;
            repository.setFilter(r -> r.toLowerCase().contains(searchTerm));
            repository.commit();
          }
        }
        """, """
        import com.webforj.data.repository.CollectionRepository;
        import java.util.List;

        class MusicView {
          private String searchTerm = "";
          private CollectionRepository<String> repository = new CollectionRepository<>(List.of());

          void onSearch(String term) {
            this.searchTerm = term;
            repository.setBaseFilter(r -> r.toLowerCase().contains(searchTerm));
            repository.commit();
          }
        }
        """));
  }

  /**
   * Reading back the current filter to check whether one is active before applying a new one.
   */
  @Test
  void renamesGetFilterToGetBaseFilter() {
    rewriteRun(java("""
        import com.webforj.data.repository.CollectionRepository;
        import java.util.List;
        import java.util.function.Predicate;

        class MusicView {
          private CollectionRepository<String> repository = new CollectionRepository<>(List.of());

          boolean isFiltered() {
            Predicate<String> current = repository.getFilter();
            return current != null;
          }
        }
        """, """
        import com.webforj.data.repository.CollectionRepository;
        import java.util.List;
        import java.util.function.Predicate;

        class MusicView {
          private CollectionRepository<String> repository = new CollectionRepository<>(List.of());

          boolean isFiltered() {
            Predicate<String> current = repository.getBaseFilter();
            return current != null;
          }
        }
        """));
  }

  /**
   * Inspecting the current sort order.
   */
  @Test
  void renamesGetOrderByToGetOrderCriteriaList() {
    rewriteRun(java("""
        import com.webforj.data.repository.CollectionRepository;
        import java.util.Comparator;
        import java.util.List;

        class MusicView {
          private CollectionRepository<String> repository = new CollectionRepository<>(List.of());

          void printSort() {
            Comparator<String> current = repository.getOrderBy();
            System.out.println(current);
          }
        }
        """, """
        import com.webforj.data.repository.CollectionRepository;
        import com.webforj.data.repository.OrderCriteriaList;

        import java.util.Comparator;
        import java.util.List;

        class MusicView {
          private CollectionRepository<String> repository = new CollectionRepository<>(List.of());

          void printSort() {
            OrderCriteriaList current = repository.getOrderCriteriaList();
            System.out.println(current);
          }
        }
        """));
  }

  /**
   * Looking up a single item by criteria.
   */
  @Test
  void renamesFindOneByToFindBy() {
    rewriteRun(java(
        """
            import com.webforj.data.repository.CollectionRepository;
            import java.util.List;
            import java.util.Optional;

            class MusicView {
              private CollectionRepository<String> repository = new CollectionRepository<>(List.of("a", "b"));

              Optional<String> findFirst() {
                return repository.findOneBy(repository);
              }
            }
            """,
        """
            import com.webforj.data.repository.CollectionRepository;
            import java.util.List;
            import java.util.Optional;

            class MusicView {
              private CollectionRepository<String> repository = new CollectionRepository<>(List.of("a", "b"));

              Optional<String> findFirst() {
                return repository.findBy(repository);
              }
            }
            """));
  }
}
