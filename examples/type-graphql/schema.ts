import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql";
import { GraphQLJSON } from "graphql-scalars";
import "reflect-metadata";

@ObjectType()
export class User {
    @Field(() => String)
    id!: string;
    @Field(() => Date)
    createdAt!: Date;
    @Field(() => Date)
    updatedAt!: Date;
    @Field(() => String)
    name!: string;
    @Field(() => String)
    email!: string;
    @Field(() => String, { nullable: true })
    bio?: string;
}

@ObjectType()
export class Post {
    @Field(() => String)
    id!: string;
    @Field(() => Date)
    createdAt!: Date;
    @Field(() => Date)
    updatedAt!: Date;
    @Field(() => String)
    title!: string;
    @Field(() => String)
    content!: string;
    @Field(() => Boolean)
    published!: boolean;
    @Field(() => String)
    authorId!: string;
}

export enum Status {
    DRAFT = "DRAFT",
    PUBLISHED = "PUBLISHED",
    ARCHIVED = "ARCHIVED"
}

registerEnumType(Status, {
  name: 'Status',
})
