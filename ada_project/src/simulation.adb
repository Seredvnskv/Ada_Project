-- SYMULACJA RESTAURACJI GRUPA ADA024

-- SAMBOR SEREDYNSKI
-- MACIEJ KOWALCZYK 
-- ZOFIA KRZESZOWIEC

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is

   ----GLOBAL VARIABLES---

   Number_Of_Producers: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;

   subtype Producer_Type is Integer range 1 .. Number_Of_Producers;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;

   -- each Producer is assigned a Product that it produces
   Product_Name: constant array (Producer_Type) of String(1 .. 8)
     := ("Pizza   ", "Napoj   ", "Salatka ", "Kanapka ", "Burger  ");
   -- Assembly is a collection of products
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 10)
     := ("Klasyczny ", "Vegge     ", "Dzieciecy ");

   ----TASK DECLARATIONS----

   -- Producer prepares specific dish
   task type Producer is
      entry Start(Product: in Producer_Type; Production_Time: in Integer);
   end Producer;

   -- Consumer receives an order made up of several dishes from the buffer
   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;
   
   task type Furious_Worker is
      entry Start;
   end Furious_Worker; 
   
   -- Buffer receives dishes from producers and serves orders to consumers
   task type Buffer is
      -- Accept a dish into the kitchen (if there's enough space)
      entry Take(Product: in Producer_Type; Number: in Integer; Product_Demand: out Integer);
      -- Serve an order (if enough dishes are available)
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
      entry Quarrel_In_Storage;
   end Buffer;

   P: array ( 1 .. Number_Of_Producers ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   FW: Furious_Worker;
   B: Buffer;

   ----TASK DEFINITIONS----

   --Producer--

  task body Producer is
     subtype Production_Time_Range is Integer range 2 .. 4;
     package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);
     -- random number generator
     G: Random_Production.Generator;
     Producer_Type_Number: Integer;
     Product_Number: Integer;
     Production: Integer;
     Random_Time: Duration;
     Demand: Integer;
  begin
   accept Start(Product: in Producer_Type; Production_Time: in Integer) do
      Random_Production.Reset(G);
      Product_Number := 1;
      Producer_Type_Number := Product;
      Production := Production_Time;
      Demand:= 100;
   end Start;

   Put_Line(ESC & "[93m" & "P: Szef kuchni rozpoczal przygotowywanie " & Product_Name(Producer_Type_Number) & ESC & "[0m");

   loop
      Random_Time := Duration(Random_Production.Random(G));

      delay Random_Time;
      Put_Line(ESC & "[93m" & "P: Przygotowano danie: " & Product_Name(Producer_Type_Number)
               & " nr "  & Integer'Image(Product_Number) & ESC & "[0m");
      
      -- Proba dostarczenia dania na wydawke
      loop
            select
               -- Sprobuj dostarczyc danie do bufora (kuchni)
               B.Take(Producer_Type_Number, Product_Number, Demand);
               Product_Number := Product_Number + 1;
               Put_Line(ESC & "[93m" & "P: Dostarczono na wydawke: " & Product_Name(Producer_Type_Number) & ESC & "[0m");
               if Demand <=0 then
                  Put_Line(ESC & "[93m" & "P: Male zapotrzebowanie na: " & Product_Name(Producer_Type_Number)
                           & ". Kucharz moze odpoczac" & ESC & "[0m");
                  delay 3.0;
               end if;
               exit;
            else
               -- Jesli kuchnia nie moze przyjac dania, odczekaj chwile i sprobuj ponownie
               Put_Line(ESC & "[93m" & "P: Wydawka zajeta, ponawiam probe dostarczenia: " & Product_Name(Producer_Type_Number) & ESC & "[0m");
               delay 2.0;
            end select;
      end loop;
   end loop;
end Producer;

task body Consumer is
   subtype Consumption_Time_Range is Integer range 4 .. 8;
   package Random_Consumption is new
     Ada.Numerics.Discrete_Random(Consumption_Time_Range);

   package Random_Assembly is new
     Ada.Numerics.Discrete_Random(Assembly_Type);

   G: Random_Consumption.Generator;
   GA: Random_Assembly.Generator;
   Consumer_Nb: Consumer_Type;
   Assembly_Number: Integer;
   Consumption: Integer;
   Assembly_Type: Integer;
   Consumer_Name: constant array (1 .. Number_Of_Consumers)
     of String(1 .. 8)
     := ("Klient_1", "Klient_2");
begin
   accept Start(Consumer_Number: in Consumer_Type;
                Consumption_Time: in Integer) do
      Random_Consumption.Reset(G);
      Random_Assembly.Reset(GA);
      Consumer_Nb := Consumer_Number;
      Consumption := Consumption_Time;
   end Start;

   Put_Line(ESC & "[96m" & "C: Klient " & Consumer_Name(Consumer_Nb) & " wszedl do restauracji" & ESC & "[0m");
   loop
      delay Duration(Random_Consumption.Random(G)); -- symulacja czasu konsumpcji
      Assembly_Type := Random_Assembly.Random(GA);
      loop
            -- proba odebrania zamowienia
            B.Deliver(Assembly_Type, Assembly_Number);

            -- Sprawdzenie, czy zamowienie numer 0 nie zostalo wydane
            if Assembly_Number /= 0 then
               Put_Line(ESC & "[96m" & "C: Klient " & Consumer_Name(Consumer_Nb) & " odebral zamowienie: " &
                          Assembly_Name(Assembly_Type) & " nr " &
                          Integer'Image(Assembly_Number) & ESC & "[0m");
               exit;
            else
               Put_Line(ESC & "[96m" & "C: Klient " & Consumer_Name(Consumer_Nb) & " chcial zamowic " &
                          Assembly_Name(Assembly_Type) & ", ale brakuje skladnikow, czeka..." & ESC & "[0m");
               delay 5.0;
            end if;
      end loop;
   end loop;
end Consumer;

   -- Furious Worker 
   
   task body Furious_Worker is
      subtype Fury_Level_Range is Integer range 0 .. 10;
      package Random_Fury is new Ada.Numerics.Discrete_Random(Fury_Level_Range);
      G: Random_Fury.Generator;
      Fury_Level: Integer;
   begin
      accept Start do 
         Random_Fury.Reset(G);
      end Start; 
   
      loop 
         delay 5.0;  -- Czas pracy pracownika
         Fury_Level := Random_Fury.Random(G);
         Put_Line(ESC & "[35m" & "FW: Furia Jana osiaga poziom " & Integer'Image(Fury_Level) & ESC & "[0m");
         
         if Fury_Level > 8 then
            Put_Line(ESC & "[35m" & "FW: Janek jest wsciekly, zaraz zacznie rzucac talerzami!" & ESC & "[0m");
            B.Quarrel_In_Storage;
         end if;
      end loop;
   end Furious_Worker;
   
   
--Buffer--

task body Buffer is
   Storage_Capacity: constant Integer := 30;
   type Storage_type is array (Producer_Type) of Integer;
   Storage: Storage_type := (0, 0, 0, 0, 0);
   Assembly_Content: array(Assembly_Type, Producer_Type) of Integer
     := ((1, 2, 1, 0, 2),
         (0, 1, 3, 1, 0),
         (1, 1, 0, 1, 1));
   Max_Assembly_Content: array(Producer_Type) of Integer;
   Max_Content: array(Producer_Type) of Integer;
   Assembly_Number: array(Assembly_Type) of Integer := (1, 1, 1);
   In_Storage: Integer := 0;
   
   procedure Throwing_Products is
   begin
         for W in Producer_Type loop
          In_Storage := In_Storage - (Storage(W) - (Storage(W) / 2));
          Storage(W) := Storage(W) / 2;
          Put_Line(ESC & "[35m" & "B: Stan kuchni po furii " & Integer'Image(Storage(W)) & " " & Product_Name(W) & ESC & "[0m");
      end loop;
         Put_Line(ESC & "[35m" & "B: Po furii Jana stracilismy polowe produktow w kuchni!" & ESC & "[0m");
         Put_Line(ESC & "[35m" & "| Liczba dan w kuchni po furri: " & Integer'Image(In_Storage) & ESC & "[0m");
   end Throwing_Products;   
      
   procedure Setup_Variables is
   begin
      for W in Producer_Type loop
         Max_Assembly_Content(W) := 0;
         for Z in Assembly_Type loop
            if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
               Max_Assembly_Content(W) := Assembly_Content(Z, W);
            end if;
         end loop;
         Max_Content(W) := Max_Assembly_Content(W)*2+2;
      end loop;
   end Setup_Variables;

   function Can_Accept(Product: Producer_Type) return Boolean is
   begin
      if In_Storage >= Storage_Capacity then
         return False;
      else
         return True;
      end if;
   end Can_Accept;

   function Can_Deliver(Assembly: Assembly_Type) return Boolean is
   begin
      for W in Producer_Type loop
         if Storage(W) < Assembly_Content(Assembly, W) then
            return False;
         end if;
      end loop;
      return True;
   end Can_Deliver;

   procedure Storage_Contents is
   begin
      for W in Producer_Type loop
         Put_Line("| Na wydawce: " & Integer'Image(Storage(W)) & " "
                  & Product_Name(W));
      end loop;
      Put_Line("| Liczba dan gotowych do wydania: " & Integer'Image(In_Storage));
   end Storage_Contents;

   procedure Clear_Excess_Products is
   begin
      for W in Producer_Type loop
         if Storage(W) > 5 then
            In_Storage := In_Storage - (Storage(W) - 5);  -- Zmniejszamy liczbe dan w kuchni
            Storage(W) := 5;  -- Ograniczamy ilosc do 5 sztuk
            Put_Line(ESC & "[91m" & "B: Nadmiar " & Product_Name(W) & " zostal usuniety z kuchni" & ESC & "[0m");
         end if;
      end loop;
   end Clear_Excess_Products;

begin
   Put_Line(ESC & "[91m" & "B: Wydawka gotowa do pracy" & ESC & "[0m");
   Setup_Variables;
   loop
      select
         accept Take(Product: in Producer_Type; Number: in Integer; Product_Demand: out Integer) do
            if Can_Accept(Product) then
               Put_Line(ESC & "[91m" & "B: Przyjeto danie: " & Product_Name(Product) & " nr " &
                          Integer'Image(Number)& ESC & "[0m");
               Storage(Product) := Storage(Product) + 1;
               In_Storage := In_Storage + 1;
            else
               Put_Line(ESC & "[91m" & "B: Wydawka pelna, usuwam nieswieze dania" & ESC & "[0m");
               Clear_Excess_Products;  -- Ograniczamy dania do 5 sztuk
               -- Sprobuj ponownie przyjac danie po wyczyszczeniu kuchni
               if Can_Accept(Product) then
                  Put_Line(ESC & "[91m" & "B: Przyjeto danie: " & Product_Name(Product) & " nr " &
                             Integer'Image(Number) & ESC & "[0m");
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
                  Put_Line(ESC & "[91m" & "B: Nadal brak miejsca na: " & Product_Name(Product) & " nr " &
                             Integer'Image(Number) & ESC & "[0m");
               end if;
            end if;
            Product_Demand := Max_Content(Product) -  Storage(Product);  
         end Take;
         
      or

         accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
            if Can_Deliver(Assembly) then
               Put_Line(ESC & "[92m" & "B: Serwuje zamowienie: " & Assembly_Name(Assembly) & ESC & "[0m");
               for W in Producer_Type loop
                  Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                  In_Storage := In_Storage - Assembly_Content(Assembly, W);
               end loop;
               Number := Assembly_Number(Assembly);
               Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
            else
               Put_Line(ESC & "[92m" & "B: Nie mozna zrealizowac zamowienia: " & Assembly_Name(Assembly) &
                          ", brakuje skladnikow!" & ESC & "[0m");
               Number := 0;
            end if;
            Storage_Contents;
         end Deliver;
         
      or
         accept Quarrel_In_Storage do
            Put_Line(ESC & "[35m" & "B: Rozpoczyna sie klotnia w kuchnii..." & ESC & "[0m");
            Throwing_Products;
         end Quarrel_In_Storage;   
            
      or
         delay 1.0;
         
      end select;
   end loop;
end Buffer;

---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Number_Of_Producers loop
      P(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,12);
   end loop;
   FW.Start;
end Simulation;
