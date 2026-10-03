package com.webforj.bbj.database;

import com.webforj.Environment;

import java.sql.Connection;

/**
 * Provides JDBC connections to BBj databases.
 *
 * @author Stephan Wald
 * @since 0.008
 */
public class JDBCConnection {

  private JDBCConnection() {}

  public static Connection getJDBCConnection(String databasename) throws Exception {
    return Environment.getCurrent().getBBjAPI().getJDBCConnection(databasename);
  }
}
