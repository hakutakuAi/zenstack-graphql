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
    email!: string;
    @Field(() => String)
    name!: string;
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
    @Field(() => Int)
    viewCount!: number;
    @Field(() => String)
    authorId!: string;
}

@ObjectType()
export class Category {
    @Field(() => String)
    id!: string;
    @Field(() => String)
    name!: string;
    @Field(() => String, { nullable: true })
    description?: string;
}

@ObjectType()
export class CategoryOnPost {
    @Field(() => String)
    postId!: string;
    @Field(() => String)
    categoryId!: string;
    @Field(() => Date)
    assignedAt!: Date;
}

@ObjectType()
export class Comment {
    @Field(() => String)
    id!: string;
    @Field(() => Date)
    createdAt!: Date;
    @Field(() => String)
    content!: string;
    @Field(() => String)
    postId!: string;
    @Field(() => String)
    authorId!: string;
}
