package com.webforj.automation.see.component.table;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import com.webforj.data.HasEntityKey;
import java.util.List;
import org.junit.jupiter.api.Test;

class TableSnapshotSerializerTest {

  @Test
  void shouldDescribeTheColumnsAndRowsWithoutTheSelectionByDefault() {
    Table<Invoice> invoices = createInvoices();

    assertEquals("""
        - Table [itemType=Invoice] [multiple]:
          - Column [id=customer]
          - Column [id=total] [label=Amount] [sort=asc] [sortable]
          - Column [id=note] [hidden]
          - Row [key=3] [customer=Grace] [total=7] [note=none]
          - Row [key=2] [customer=Linus] [total=15] [note=none]
          - …
        """, Snapshot.render(invoices, new SnapshotOptions().setMaxWidth(2)));
  }

  @Test
  void shouldFlagTheSelectedRowWithBrowserState() {
    Table<Invoice> invoices = createInvoices();

    assertEquals("""
        - Table [itemType=Invoice] [multiple]:
          - Column [id=customer]
          - Column [id=total] [label=Amount] [sort=asc] [sortable]
          - Column [id=note] [hidden]
          - Row [key=3] [customer=Grace] [total=7] [note=none]
          - Row [key=2] [customer=Linus] [total=15] [note=none] [selected]
          - …
        """, Snapshot.render(invoices, new SnapshotOptions().setMaxWidth(2).setBrowserState(true)));
  }

  @Test
  void shouldLeaveOutTheGeneratedKeyOfTheItemWithoutOne() {
    Table<String> names = new Table<>();
    names.addColumn("name", String::valueOf);
    names.setItems(List.of("Ada"));

    assertEquals("""
        - Table [itemType=String]:
          - Column [id=name]
          - Row [name=Ada]
        """, Snapshot.render(names));
  }

  @Test
  void shouldDescribeTheEmptyTableByItsColumns() {
    Table<String> names = new Table<>();
    names.addColumn("name", String::valueOf);

    assertEquals("""
        - Table:
          - Column [id=name]
        """, Snapshot.render(names));
  }

  private static Table<Invoice> createInvoices() {
    Table<Invoice> invoices = new Table<>();
    invoices.setSelectionMode(Table.SelectionMode.MULTIPLE);
    invoices.addColumn("customer", Invoice::getCustomer);
    invoices.addColumn("total", Invoice::getTotal).setLabel("Amount").setSortable(true)
        .setSortDirection(Column.SortDirection.ASC);
    invoices.addColumn("note", Invoice::getNote).setHidden(true);
    Invoice second = new Invoice(2, "Linus", 15, "none");
    invoices.setItems(
        List.of(new Invoice(1, "Ada", 40, "urgent"), second, new Invoice(3, "Grace", 7, "none")));
    invoices.select(second);

    return invoices;
  }

  private static final class Invoice implements HasEntityKey {
    private final int id;
    private final String customer;
    private final int total;
    private final String note;

    private Invoice(int id, String customer, int total, String note) {
      this.id = id;
      this.customer = customer;
      this.total = total;
      this.note = note;
    }

    @Override
    public Object getEntityKey() {
      return id;
    }

    private String getCustomer() {
      return customer;
    }

    private int getTotal() {
      return total;
    }

    private String getNote() {
      return note;
    }
  }
}
