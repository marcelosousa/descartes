/* ./apache-storm-44e9aaf/storm-core/src/jvm/backtype/storm/generated/LSWorkerHeartbeat.java */
/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/**
 * Autogenerated by Thrift Compiler (0.9.2)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package backtype.storm.generated;

import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.SchemeFactory;
import org.apache.thrift.scheme.StandardScheme;

import org.apache.thrift.scheme.TupleScheme;
import org.apache.thrift.protocol.TTupleProtocol;
import org.apache.thrift.protocol.TProtocolException;
import org.apache.thrift.EncodingUtils;
import org.apache.thrift.TException;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.server.AbstractNonblockingServer.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.Set;
import java.util.HashSet;
import java.util.EnumSet;
import java.util.Collections;
import java.util.BitSet;
import java.nio.ByteBuffer;
import java.util.Arrays;
import javax.annotation.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings({"cast", "rawtypes", "serial", "unchecked"})
@Generated(value = "Autogenerated by Thrift Compiler (0.9.2)", date = "2015-4-10")
public class LSWorkerHeartbeat implements org.apache.thrift.TBase<LSWorkerHeartbeat, LSWorkerHeartbeat._Fields>, java.io.Serializable, Cloneable, Comparable<LSWorkerHeartbeat> {
  boolean is_set_topology_id();
  boolean is_set_executors();
  int time_secs;
  Id topology_id;
  int port;
  Executors executors;

  public boolean equals(LSWorkerHeartbeat o1, LSWorkerHeartbeat o2) {
    boolean this_present_time_secs = true;
    boolean that_present_time_secs = true;
    if (this_present_time_secs || that_present_time_secs) {
      if (!(this_present_time_secs && that_present_time_secs))
        return false;
      if (o1.time_secs != o2.time_secs)
        return false;
    }

    boolean this_present_topology_id = true && o1.is_set_topology_id();
    boolean that_present_topology_id = true && o2.is_set_topology_id();
    if (this_present_topology_id || that_present_topology_id) {
      if (!(this_present_topology_id && that_present_topology_id)) {
        return false;
      }
      if (equals(o1.topology_id, o2.topology_id) == 0) {
        return false;
      }
    }

    boolean this_present_executors = true && o1.is_set_executors();
    boolean that_present_executors = true && o2.is_set_executors();
    if (this_present_executors || that_present_executors) {
      if (!(this_present_executors && that_present_executors)) {
        return false;
      }
      if (equals(o1.executors, o2.executors) == 0) {
        return false;
      }
    }

    boolean this_present_port = true;
    boolean that_present_port = true;
    if (this_present_port || that_present_port) {
      if (!(this_present_port && that_present_port)) {
        return false;
      }
      if (o1.port != o2.port) {
        return false;
      }
    }

    return true;
  }

}

