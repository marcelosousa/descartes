/* Soot - a J*va Optimization Framework
 * Copyright (C) 2008 Eric Bodden
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
import java.util.Map;

import soot.jimple.*;
import soot.Body;
import soot.BodyTransformer;
import soot.G;
import soot.PackManager;
import soot.Transform;
import soot.Unit;
import soot.toolkits.graph.DirectedGraph;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.util.cfgcmd.CFGToDotGraph;
import soot.toolkits.scalar.BackwardFlowAnalysis;
import soot.Local;

import com.microsoft.z3.*;
import com.microsoft.z3.Expr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

public class MyMain {

    public static void main(String[] args) {
        PackManager.v().getPack("jtp").add(
                                           new Transform("jtp.myTransform", new BodyTransformer() {

                                                   protected void internalTransform(Body body, String phase, Map options) {
                                                       String name = body.getMethod().getName();
                                                       ExceptionalUnitGraph eug = new ExceptionalUnitGraph(body);
                                                       MyAnalysis02 b = new MyAnalysis02(eug);
                                                       List<Unit> heads = eug.getHeads();
                                                       try {
                                                       for (Iterator<Unit> i1 = heads.iterator(); i1.hasNext(); ) {
                                                           Unit u1 = i1.next();
                                                           AnalysisContainer conds = b.getFlowAfter(u1);
                                                           G.v().out.println("The flow at this head is " + conds.toString());
                                                           if (name.equals("compare")) {

                                                               String p0 = body.getParameterLocal(0).toString();
                                                               String p1 = body.getParameterLocal(1).toString();
                                                               IntExpr e0=b.m1.get(p0);
                                                               IntExpr e1=b.m1.get(p1);
                                                               IntExpr e[] = new IntExpr[3]; String o[] = new String[3];
                                                               for (int i2 = 0; i2 < 3; ++i2) {
                                                                   o[i2] = i2 < 1 ? "o": (o[i2 - 1] + "_"); while (b.m1.containsKey(o[i2])) o[i2] += "_";
                                                                   b.m1.put(o[i2], e[i2] = b.ctx.MkIntConst(o[i2]));
                                                               }
                                                               BoolExpr reflexCond = (BoolExpr) conds.eq0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[0]});
                                                               BoolExpr symmCond = b.ctx.MkIff(
                                                                                               (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[1]}),
                                                                                               (BoolExpr) conds.gt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[1], e[0]}));
                                                               BoolExpr transCond = b.ctx.MkImplies(b.ctx.MkAnd(new BoolExpr[]
                                                                   {(BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[1]}),
                                                                    (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[1], e[2]})}),
                                                                                                    (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[2]}));
                                                               Solver solver = b.ctx.MkSolver();
                                                               solver.Push(); solver.Assert(b.ctx.MkNot(reflexCond));
                                                               G.v().out.println("reflexCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                               solver.Pop();
                                                               solver.Push(); solver.Assert(b.ctx.MkNot(symmCond));
                                                               G.v().out.println("symmCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                               solver.Pop();
                                                               solver.Push(); solver.Assert(b.ctx.MkNot(transCond));
                                                               G.v().out.println("transCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                               solver.Pop();
                                                               
                                                           }
                                                       }
                                                       } catch (Z3Exception E) {
                                                           E.printStackTrace();
                                                       }

                                                     if (name.equals("compare")) {
                                                           G.v().out.println("Here's a compare body.");
                                                           for (Iterator<Unit> i1 = eug.iterator();
                                                                i1.hasNext();
                                                                ) {
                                                               Unit u1 = i1.next();
                                                               if (u1 instanceof IfStmt)
                                                                   G.v().out.println("Here's an if statement unit, with condition\n" +
                                                                                     ((IfStmt)u1).getCondition().toString() +
                                                                                     "\nand target\n" +
                                                                                     ((IfStmt)u1).getTarget().toString());
                                                               else if (u1 instanceof DefinitionStmt)
                                                                   G.v().out.println("Here's a definition statement unit, with left op " +
                                                                                     ((DefinitionStmt)u1).getLeftOp().toString() + ", which " +
                                                                                     (((DefinitionStmt)u1).getLeftOp() instanceof Local ? "is": "is not") +
                                                                                     " a local, and right op " +
                                                                                     ((DefinitionStmt)u1).getRightOp().toString() + ", which " +
                                                                                     (((DefinitionStmt)u1).getRightOp() instanceof IntConstant ? "is" : "is not") +
                                                                                     " an integer constant and " +
                                                                                     (((DefinitionStmt)u1).getRightOp() instanceof Local ? "is" : "is not")
                                                                                     + " a local and " +
                                                                                     (((DefinitionStmt)u1).getRightOp() instanceof InstanceFieldRef ? "is" : "is not") +
                                                                                     " an instance field reference.");
                                                               else if (u1 instanceof ReturnStmt)
                                                                   G.v().out.println("Here's a return statement unit, with op " +
                                                                                     ((ReturnStmt)u1).getOp().toString() + ", which " + (((ReturnStmt)u1).getOp() instanceof Local ? "is" : "is not") + " a local.");
                                                               else if (u1 instanceof GotoStmt)
                                                                   G.v().out.println("Here's a goto statement unit, with target " +
                                                                                     ((GotoStmt)u1).getTarget().toString());
                                                               else {
                                                                   G.v().out.println("Here's a unit.");
                                                                   G.v().out.println(u1.toString());
                                                               }
                                                               G.v().out.println("The flow at this unit is " + b.getFlowAfter(u1));
                                                           }
                                                           (new CFGToDotGraph()).drawCFG(eug, body).plot("/tmp/"+name+".dot");
                                                       } else if (false) {
                                                           G.v().out.println("Here's a body.");

                                                           for (Iterator<Unit> i1 = eug.iterator();
                                                                i1.hasNext();
                                                                ) {
                                                               Unit u1 = i1.next();
                                                               G.v().out.println("Here's a unit.");
                                                               G.v().out.println(u1.toString());

                                                           }
                                                       }

                                                       // use G.v().out instead of System.out so that Soot can
                                                       // redirect this output to the Eclipse console
                                                       G.v().out.println(body.getMethod());
                                                   }

                                               }));
		
        soot.Main.main(args);
    }

    public static class AnalysisContainer {
        BoolExpr lt0;
        BoolExpr eq0;
        BoolExpr gt0;

        @Override
        public String toString() {
            return "{" + this.lt0.toString() + ", " + this.eq0.toString() + ", " + this.gt0.toString() + "}";
        }

    }

    public static class MyAnalysis02 extends BackwardFlowAnalysis<Unit, AnalysisContainer> {

        Context ctx;
        Map<String, IntExpr> m1;
        FuncDecl Field;

        public MyAnalysis02(DirectedGraph<Unit> graph) {
            super(graph);

            HashMap<String, String> cfg = new HashMap<String, String>();
            cfg.put("model", "true");
            try {
                ctx = new Context(cfg);
                Field = ctx.MkFuncDecl("Field", new Sort[] {ctx.IntSort(), ctx.IntSort()}, ctx.IntSort());
            } catch (Z3Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            m1 = new HashMap<String, IntExpr>();
            //set up buncha data structures

            doAnalysis();
        }

        @Override
            protected void flowThrough(AnalysisContainer arg0, Unit arg1, AnalysisContainer arg2) {
            // TODO Auto-generated method stub
            try {
                if (arg1 instanceof ReturnStmt) {
                    String name = ((ReturnStmt)arg1).getOp().toString();
                    IntExpr temp1 = ctx.MkIntConst(name);
                    m1.put(name, temp1);
                    arg2.lt0 = ctx.MkLt(temp1, ctx.MkInt(0));
                    arg2.eq0 = ctx.MkEq(temp1, ctx.MkInt(0));
                    arg2.gt0 = ctx.MkGt(temp1, ctx.MkInt(0));

                } else if (arg1 instanceof DefinitionStmt) {
                    DefinitionStmt arg11 = ((DefinitionStmt)arg1);
                    Expr temp2 = m1.get(arg11.getLeftOp().toString());
                    if (temp2 != null) {
                        if (arg11.getRightOp() instanceof IntConstant ||
                            arg11.getRightOp() instanceof InstanceFieldRef ||
                            arg11.getRightOp() instanceof Local) {
                            IntExpr temp1;
                            if (arg11.getRightOp() instanceof IntConstant)
                                temp1 = ctx.MkInt(((IntConstant) arg11.getRightOp()).value);
                            else if (arg11.getRightOp() instanceof InstanceFieldRef) {
                                String name3 = ((InstanceFieldRef)arg11.getRightOp()).getBase().toString();
                                IntExpr temp3 = m1.get(name3);
                                if (temp3 == null) {
                                    temp3 = ctx.MkIntConst(name3);
                                    m1.put(name3, temp3);
                                }
                                String name4 = ((InstanceFieldRef)arg11.getRightOp()).getField().toString();
                                IntExpr temp4 = m1.get(name4);
                                if (temp4 == null) {
                                    temp4 = ctx.MkIntConst(name4);
                                    m1.put(name4, temp4);
                                }
                                temp1 = (IntExpr) ctx.MkApp(Field, new Expr[] {temp3, temp4});
                            } else /*Local*/ {
                                String name = arg11.getRightOp().toString();
                                temp1 = m1.get(name);
                                if (temp1 == null) {
                                    temp1 = ctx.MkIntConst(name);
                                    m1.put(name, temp1);
                                }
                            }
                            Expr[] temp3 = new Expr[] {temp2}, temp4 = new Expr[] {temp1};
                            arg2.lt0 = (BoolExpr) arg0.lt0.Substitute(temp3, temp4);
                            arg2.eq0 = (BoolExpr) arg0.eq0.Substitute(temp3, temp4);
                            arg2.gt0 = (BoolExpr) arg0.gt0.Substitute(temp3, temp4);
                        } else
                            copy(arg0, arg2);
                    } else
                        copy(arg0, arg2);
                } else 
                    copy(arg0, arg2);
            } catch (Z3Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }			
        }

        @Override
            protected void copy(AnalysisContainer arg0, AnalysisContainer arg1) {
            arg1.lt0 = arg0.lt0;
            arg1.eq0 = arg0.eq0;
            arg1.gt0 = arg0.gt0;
        }

        @Override
            protected AnalysisContainer entryInitialFlow() {
            AnalysisContainer temp1 = new AnalysisContainer ();
            try {
                temp1.lt0 = this.ctx.MkTrue();
                temp1.eq0 = this.ctx.MkTrue();
                temp1.gt0 = this.ctx.MkTrue();
            } catch (Z3Exception e) {
                e.printStackTrace();
            } finally {
                return temp1;
            }
        }

        @Override
            protected void merge(AnalysisContainer arg0, AnalysisContainer arg1, AnalysisContainer arg2) {
            try {
                arg2.lt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.lt0, arg0.lt0});
                arg2.eq0 = this.ctx.MkAnd(new BoolExpr[] {arg1.eq0, arg0.eq0});
                arg2.gt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.gt0, arg0.gt0});
            } catch (Z3Exception e) {
                e.printStackTrace();
            }
        }

        @Override
            protected void merge(Unit arg0, AnalysisContainer arg1, AnalysisContainer arg2, AnalysisContainer arg3) {
            try {
                if (arg0 instanceof IfStmt) {
                    BoolExpr temp3 = null;
                    if (((IfStmt)arg0).getCondition() instanceof BinopExpr) {
                        String name1 = ((BinopExpr)((IfStmt)arg0).getCondition()).getOp1().toString();
                        IntExpr temp1 = m1.get(name1);
                        if (temp1 == null) {
                            temp1 = ctx.MkIntConst(name1);
                            m1.put(name1, temp1);
                        }
                        String name2 = ((BinopExpr)((IfStmt)arg0).getCondition()).getOp2().toString();
                        IntExpr temp2 = m1.get(name2);
                        if (temp2 == null) {
                            temp2 = ctx.MkIntConst(name2);
                            m1.put(name2, temp2);
                        }
                        if (((IfStmt)arg0).getCondition() instanceof EqExpr) {
                            temp3 = ctx.MkEq(temp1, temp2);
                        } else if (((IfStmt)arg0).getCondition() instanceof GeExpr) {
                            temp3 = ctx.MkGe(temp1, temp2);
                        } else if (((IfStmt)arg0).getCondition() instanceof GtExpr) {
                            temp3 = ctx.MkGt(temp1, temp2);
                        } else if (((IfStmt)arg0).getCondition() instanceof LeExpr) {
                            temp3 = ctx.MkLe(temp1, temp2);
                        } else if (((IfStmt)arg0).getCondition() instanceof LtExpr) {
                            temp3 = ctx.MkLt(temp1, temp2);
                        } else if (((IfStmt)arg0).getCondition() instanceof LtExpr) {
                            temp3 = ctx.MkLt(temp1, temp2);
                        } else if (((IfStmt)arg0).getCondition() instanceof NeExpr) {
                            temp3 = ctx.MkNot(ctx.MkEq(temp1, temp2));
                        }
                        BoolExpr temp4 = ctx.MkNot(temp3);
                        arg3.lt0 = (BoolExpr) this.ctx.MkAnd(new BoolExpr[] {this.ctx.MkImplies(temp3, arg2.lt0), this.ctx.MkImplies(temp4, arg1.lt0)});
                        arg3.eq0 = (BoolExpr) this.ctx.MkAnd(new BoolExpr[] {this.ctx.MkImplies(temp3, arg2.eq0), this.ctx.MkImplies(temp4, arg1.eq0)});
                        arg3.gt0 = (BoolExpr) this.ctx.MkAnd(new BoolExpr[] {this.ctx.MkImplies(temp3, arg2.gt0), this.ctx.MkImplies(temp4, arg1.gt0)});
                    }
                } else {
                    arg3.lt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.lt0, arg2.lt0});
                    arg3.eq0 = this.ctx.MkAnd(new BoolExpr[] {arg1.eq0, arg2.eq0});
                    arg3.gt0 = this.ctx.MkAnd(new BoolExpr[] {arg1.gt0, arg2.gt0});
                }
            } catch (Z3Exception e) {
                e.printStackTrace();
            }
        }

        @Override
            protected AnalysisContainer newInitialFlow() {
            AnalysisContainer temp1 = new AnalysisContainer ();
            try {
                temp1.lt0 = this.ctx.MkTrue();
                temp1.eq0 = this.ctx.MkTrue();
                temp1.gt0 = this.ctx.MkTrue();
            } catch (Z3Exception e) {
                e.printStackTrace();
            } finally {
                return temp1;
            }
        }
    	
    }
}
