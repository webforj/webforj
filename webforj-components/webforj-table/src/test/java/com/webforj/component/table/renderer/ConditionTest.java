package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ConditionTest {

  @Nested
  class EqualTo {

    @Test
    void shouldMatchString() {
      Condition condition = Condition.equalTo("Rock");
      assertEquals("String(cell.value) === 'Rock'", condition.toExpression());
    }

    @Test
    void shouldMatchNumber() {
      Condition condition = Condition.equalTo(42);
      assertEquals("Number(cell.value) === 42", condition.toExpression());
    }

    @Test
    void shouldEscapeSingleQuotes() {
      Condition condition = Condition.equalTo("Rock'n'Roll");
      assertEquals("String(cell.value) === 'Rock\\'n\\'Roll'", condition.toExpression());
    }

    @Test
    void shouldIgnoreCase() {
      Condition condition = Condition.equalToIgnoreCase("Rock");
      assertEquals("String(cell.value).toLowerCase() === 'rock'", condition.toExpression());
    }
  }

  @Nested
  class In {

    @Test
    void shouldMatchSingleValue() {
      Condition condition = Condition.in("Rock");
      assertEquals("['Rock'].indexOf(String(cell.value)) !== -1", condition.toExpression());
    }

    @Test
    void shouldMatchMultipleValues() {
      Condition condition = Condition.in("Rock", "Jazz", "Pop");
      assertEquals("['Rock','Jazz','Pop'].indexOf(String(cell.value)) !== -1",
          condition.toExpression());
    }

    @Test
    void shouldEscapeSingleQuotes() {
      Condition condition = Condition.in("R&B", "Rock'n'Roll");
      assertEquals("['R&B','Rock\\'n\\'Roll'].indexOf(String(cell.value)) !== -1",
          condition.toExpression());
    }

    @Test
    void shouldIgnoreCase() {
      Condition condition = Condition.inIgnoreCase("Rock", "Jazz");
      assertEquals("['rock','jazz'].indexOf(String(cell.value).toLowerCase()) !== -1",
          condition.toExpression());
    }
  }

  @Nested
  class Comparison {

    @Test
    void shouldCreateGreaterThan() {
      Condition condition = Condition.greaterThan(100);
      assertEquals("Number(cell.value) > 100", condition.toExpression());
    }

    @Test
    void shouldCreateGreaterThanOrEqual() {
      Condition condition = Condition.greaterThanOrEqual(100);
      assertEquals("Number(cell.value) >= 100", condition.toExpression());
    }

    @Test
    void shouldCreateLessThan() {
      Condition condition = Condition.lessThan(50);
      assertEquals("Number(cell.value) < 50", condition.toExpression());
    }

    @Test
    void shouldCreateLessThanOrEqual() {
      Condition condition = Condition.lessThanOrEqual(50);
      assertEquals("Number(cell.value) <= 50", condition.toExpression());
    }

    @Test
    void shouldCreateBetween() {
      Condition condition = Condition.between(10, 50);
      assertEquals("Number(cell.value) >= 10 && Number(cell.value) <= 50",
          condition.toExpression());
    }

    @Test
    void shouldHandleDecimals() {
      Condition condition = Condition.greaterThan(10.5);
      assertEquals("Number(cell.value) > 10.5", condition.toExpression());
    }

    @Test
    void shouldHandleBetweenWithDecimals() {
      Condition condition = Condition.between(1.5, 9.99);
      assertEquals("Number(cell.value) >= 1.5 && Number(cell.value) <= 9.99",
          condition.toExpression());
    }
  }

  @Nested
  class BooleanAndEmpty {

    @Test
    void shouldCreateIsEmpty() {
      Condition condition = Condition.isEmpty();
      assertEquals("cell.value == null || cell.value === '' || cell.value === undefined",
          condition.toExpression());
    }

    @Test
    void shouldCreateIsTrue() {
      Condition condition = Condition.isTrue();
      assertEquals("cell.value === true || cell.value === 'true'", condition.toExpression());
    }

    @Test
    void shouldCreateIsFalse() {
      Condition condition = Condition.isFalse();
      assertEquals("cell.value === false || cell.value === 'false'", condition.toExpression());
    }
  }

  @Nested
  class Contains {

    @Test
    void shouldMatchText() {
      Condition condition = Condition.contains("jazz");
      assertEquals("String(cell.value).indexOf('jazz') !== -1", condition.toExpression());
    }

    @Test
    void shouldEscapeSingleQuotes() {
      Condition condition = Condition.contains("it's");
      assertEquals("String(cell.value).indexOf('it\\'s') !== -1", condition.toExpression());
    }

    @Test
    void shouldIgnoreCase() {
      Condition condition = Condition.containsIgnoreCase("Jazz");
      assertEquals("String(cell.value).toLowerCase().indexOf('jazz') !== -1",
          condition.toExpression());
    }
  }

  @Nested
  class Expression {

    @Test
    void shouldCreateRawExpression() {
      Condition condition = Condition.expression("cell.row.getValue('status') === 'active'");
      assertEquals("cell.row.getValue('status') === 'active'", condition.toExpression());
    }
  }

  @Nested
  class Composition {

    @Test
    void shouldComposeWithAnd() {
      Condition combined = Condition.greaterThan(10).and(Condition.lessThan(50));
      assertEquals("(Number(cell.value) > 10) && (Number(cell.value) < 50)",
          combined.toExpression());
    }

    @Test
    void shouldComposeWithOr() {
      Condition combined = Condition.equalTo("Rock").or(Condition.equalTo("Jazz"));
      assertEquals("(String(cell.value) === 'Rock') || (String(cell.value) === 'Jazz')",
          combined.toExpression());
    }

    @Test
    void shouldNegate() {
      Condition negated = Condition.isEmpty().negate();
      assertEquals("!(cell.value == null || cell.value === '' || cell.value === undefined)",
          negated.toExpression());
    }

    @Test
    void shouldChainAndOrNegate() {
      Condition condition = Condition.greaterThan(10).and(Condition.lessThan(50)).negate();
      assertEquals("!((Number(cell.value) > 10) && (Number(cell.value) < 50))",
          condition.toExpression());
    }
  }

  @Nested
  class Column {

    @Test
    void shouldCreateEqualToString() {
      Condition condition = Condition.column("status").equalTo("active");
      assertEquals("String(cell.row.getValue('status')) === 'active'", condition.toExpression());
    }

    @Test
    void shouldCreateEqualToNumber() {
      Condition condition = Condition.column("price").equalTo(42);
      assertEquals("Number(cell.row.getValue('price')) === 42", condition.toExpression());
    }

    @Test
    void shouldCreateEqualToIgnoreCase() {
      Condition condition = Condition.column("status").equalToIgnoreCase("Active");
      assertEquals("String(cell.row.getValue('status')).toLowerCase() === 'active'",
          condition.toExpression());
    }

    @Test
    void shouldCreateIn() {
      Condition condition = Condition.column("genre").in("Rock", "Jazz");
      assertEquals("['Rock','Jazz'].indexOf(String(cell.row.getValue('genre'))) !== -1",
          condition.toExpression());
    }

    @Test
    void shouldCreateInIgnoreCase() {
      Condition condition = Condition.column("genre").inIgnoreCase("Rock", "Jazz");
      assertEquals(
          "['rock','jazz'].indexOf(String(cell.row.getValue('genre')).toLowerCase()) !== -1",
          condition.toExpression());
    }

    @Test
    void shouldCreateGreaterThan() {
      Condition condition = Condition.column("price").greaterThan(100);
      assertEquals("Number(cell.row.getValue('price')) > 100", condition.toExpression());
    }

    @Test
    void shouldCreateBetween() {
      Condition condition = Condition.column("score").between(1, 10);
      assertEquals(
          "Number(cell.row.getValue('score')) >= 1 && Number(cell.row.getValue('score')) <= 10",
          condition.toExpression());
    }

    @Test
    void shouldCreateIsEmpty() {
      Condition condition = Condition.column("notes").isEmpty();
      assertEquals("cell.row.getValue('notes') == null || cell.row.getValue('notes') === ''"
          + " || cell.row.getValue('notes') === undefined", condition.toExpression());
    }

    @Test
    void shouldCreateIsTrue() {
      Condition condition = Condition.column("active").isTrue();
      assertEquals("cell.row.getValue('active') === true || cell.row.getValue('active') === 'true'",
          condition.toExpression());
    }

    @Test
    void shouldCreateContains() {
      Condition condition = Condition.column("description").contains("jazz");
      assertEquals("String(cell.row.getValue('description')).indexOf('jazz') !== -1",
          condition.toExpression());
    }

    @Test
    void shouldCombineWithCellConditions() {
      Condition condition =
          Condition.greaterThan(100).and(Condition.column("status").equalTo("active"));
      assertEquals(
          "(Number(cell.value) > 100) && (String(cell.row.getValue('status')) === 'active')",
          condition.toExpression());
    }

    @Test
    void shouldCombineCellValueWithColumn() {
      Condition condition = Condition.equalTo("Rock").and(Condition.column("stock").greaterThan(0));
      assertEquals("(String(cell.value) === 'Rock') && (Number(cell.row.getValue('stock')) > 0)",
          condition.toExpression());
    }
  }
}
