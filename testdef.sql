CREATE TABLE "users" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "email" TEXT NOT NULL
);
CREATE TABLE "conversazioni" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "user" INTEGER NOT NULL,
    message text not null,
    "opponent" integer,
    FOREIGN KEY(user) REFERENCES users(id),
    FOREIGN KEY(opponent) REFERENCES users(id)
);

create table "messaggi" (
        id integer primary key AUTOINCREMENT not null,
        message text not null,
        conversazione integer not null,
        FOREIGN KEY(conversazione) REFERENCES conversazioni(id)
        );

create view newconversation as select message,users.id as user from messaggi join users;

create trigger newconversation instead of insert on newconversation begin
        insert into conversazioni (user,message) values (new.user,new.message);
        end;

create view joinconversation as select conversazioni.id as conversazione, opponent from conversazioni;

create trigger joinconversation instead of insert on joinconversation begin
        select case when ((select opponent from conversazioni where id = new.conversazione) notnull) then raise (abort,"already taken") end;
        select case when ((select user from conversazioni where id = new.conversazione and new.opponent = user) notnull) then raise (abort,"same user") end;
        update conversazioni set opponent = new.opponent where conversazioni.id = new.conversazione;
        end;


create view newmessages as select message,id as conversazione,user from conversazioni;

create trigger newmessage instead of insert on newmessages begin
        select case when ((select opponent from conversazioni where id = new.conversazione and opponent = new.user ) isnull) then raise (abort,"not the opponent") end;
        insert into messaggi (conversazione,message) select new.conversazione, message from conversazioni where id = new.conversazione;
        replace into conversazioni (id,user,message,opponent) select id,opponent, new.message,user from conversazioni where id=new.conversazione;
        end;

        
