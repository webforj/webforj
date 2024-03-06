package com.webforj.bbj.database;

import com.webforj.Environment;

import java.sql.Connection;

public class JDBCConnection {

  private JDBCConnection() {}

  public static Connection getJDBCConnection(String databasename) throws Exception {
    return Environment.getCurrent().getBBjAPI().getJDBCConnection(databasename);
  }
}
