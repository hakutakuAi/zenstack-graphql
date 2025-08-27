import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql";
import { GraphQLJSON } from "graphql-scalars";
import "reflect-metadata";

@ObjectType({ description: "User of the blog system" })
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
    @Field(() => [Post])
    posts!: Post[];
    @Field(() => [Comment])
    comments!: Comment[];
}

@ObjectType({ description: "Blog post" })
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
    @Field(() => User)
    author!: User;
    @Field(() => String)
    authorId!: string;
    @Field(() => [PostCategory])
    categories!: PostCategory[];
    @Field(() => [Comment])
    comments!: Comment[];
}

@ObjectType({ description: "Post category" })
export class Category {
    @Field(() => String)
    id!: string;
    @Field(() => String)
    name!: string;
    @Field(() => String, { nullable: true })
    description?: string;
    @Field(() => [PostCategory])
    posts!: PostCategory[];
}

@ObjectType({ description: "Relation between Post and Category" })
export class PostCategory {
    @Field(() => Post)
    post!: Post;
    @Field(() => String)
    postId!: string;
    @Field(() => Category)
    category!: Category;
    @Field(() => String)
    categoryId!: string;
    @Field(() => Date)
    assignedAt!: Date;
}

@ObjectType({ description: "Comment on a blog post" })
export class Comment {
    @Field(() => String)
    id!: string;
    @Field(() => Date)
    createdAt!: Date;
    @Field(() => String)
    content!: string;
    @Field(() => Post)
    post!: Post;
    @Field(() => String)
    postId!: string;
    @Field(() => User)
    author!: User;
    @Field(() => String)
    authorId!: string;
}

@InputType()
export class ForwardPaginationInput {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
}

@InputType()
export class BackwardPaginationInput {
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class PaginationInput {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@ObjectType()
export class PageInfo {
    @Field(() => Boolean)
    hasNextPage!: boolean;
    @Field(() => Boolean)
    hasPreviousPage!: boolean;
    @Field(() => String, { nullable: true })
    startCursor?: string | undefined;
    @Field(() => String, { nullable: true })
    endCursor?: string | undefined;
}

@ObjectType()
export class UserEdge {
    @Field(() => User)
    node!: User;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class UserConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [UserEdge])
    edges!: UserEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class PostEdge {
    @Field(() => Post)
    node!: Post;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class PostConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [PostEdge])
    edges!: PostEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class CategoryEdge {
    @Field(() => Category)
    node!: Category;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class CategoryConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [CategoryEdge])
    edges!: CategoryEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class PostCategoryEdge {
    @Field(() => PostCategory)
    node!: PostCategory;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class PostCategoryConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [PostCategoryEdge])
    edges!: PostCategoryEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class CommentEdge {
    @Field(() => Comment)
    node!: Comment;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class CommentConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [CommentEdge])
    edges!: CommentEdge[];
    @Field(() => Int)
    totalCount!: number;
}

export enum SortDirection {
    ASC = "ASC",
    DESC = "DESC"
}

registerEnumType(SortDirection, {
  name: 'SortDirection',
  description: 'Sort direction for ordering results',
})

@InputType()
export class UserSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    updatedAt?: SortDirection | undefined;
}

@InputType()
export class PostSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    updatedAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    viewCount?: SortDirection | undefined;
}

@InputType()
export class CategorySortInput {
    @Field(() => SortDirection, { nullable: true })
    _placeholder?: SortDirection | undefined;
}

@InputType()
export class PostCategorySortInput {
    @Field(() => SortDirection, { nullable: true })
    _placeholder?: SortDirection | undefined;
}

@InputType()
export class CommentSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
}

@InputType()
export class NumericFilterInput {
    @Field(() => Float, { nullable: true })
    equals?: number | undefined;
    @Field(() => Float, { nullable: true })
    not?: number | undefined;
    @Field(() => Float, { nullable: true })
    gt?: number | undefined;
    @Field(() => Float, { nullable: true })
    gte?: number | undefined;
    @Field(() => Float, { nullable: true })
    lt?: number | undefined;
    @Field(() => Float, { nullable: true })
    lte?: number | undefined;
}

@InputType()
export class StringFilterInput {
    @Field(() => String, { nullable: true })
    equals?: string | undefined;
    @Field(() => String, { nullable: true })
    not?: string | undefined;
    @Field(() => [String!], { nullable: true })
    in?: string[] | undefined;
    @Field(() => [String!], { nullable: true })
    notIn?: string[] | undefined;
    @Field(() => String, { nullable: true })
    contains?: string | undefined;
    @Field(() => String, { nullable: true })
    startsWith?: string | undefined;
    @Field(() => String, { nullable: true })
    endsWith?: string | undefined;
}

@InputType()
export class BooleanFilterInput {
    @Field(() => Boolean, { nullable: true })
    equals?: boolean | undefined;
    @Field(() => Boolean, { nullable: true })
    not?: boolean | undefined;
}

@InputType()
export class DateTimeFilterInput {
    @Field(() => Date, { nullable: true })
    equals?: Date | undefined;
    @Field(() => Date, { nullable: true })
    not?: Date | undefined;
    @Field(() => Date, { nullable: true })
    gt?: Date | undefined;
    @Field(() => Date, { nullable: true })
    gte?: Date | undefined;
    @Field(() => Date, { nullable: true })
    lt?: Date | undefined;
    @Field(() => Date, { nullable: true })
    lte?: Date | undefined;
}

@InputType()
export class UserFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    email?: StringFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    name?: StringFilterInput | undefined;
    @Field(() => [UserFilterInput!], { nullable: true })
    AND?: UserFilterInput[] | undefined;
    @Field(() => [UserFilterInput!], { nullable: true })
    OR?: UserFilterInput[] | undefined;
}

@InputType()
export class PostFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    title?: StringFilterInput | undefined;
    @Field(() => BooleanFilterInput, { nullable: true })
    published?: BooleanFilterInput | undefined;
    @Field(() => [PostFilterInput!], { nullable: true })
    AND?: PostFilterInput[] | undefined;
    @Field(() => [PostFilterInput!], { nullable: true })
    OR?: PostFilterInput[] | undefined;
}

@InputType()
export class CategoryFilterInput {
    @Field(() => StringFilterInput, { nullable: true })
    name?: StringFilterInput | undefined;
    @Field(() => [CategoryFilterInput!], { nullable: true })
    AND?: CategoryFilterInput[] | undefined;
    @Field(() => [CategoryFilterInput!], { nullable: true })
    OR?: CategoryFilterInput[] | undefined;
}

@InputType()
export class PostCategoryFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    assignedAt?: DateTimeFilterInput | undefined;
    @Field(() => [PostCategoryFilterInput!], { nullable: true })
    AND?: PostCategoryFilterInput[] | undefined;
    @Field(() => [PostCategoryFilterInput!], { nullable: true })
    OR?: PostCategoryFilterInput[] | undefined;
}

@InputType()
export class CommentFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    content?: StringFilterInput | undefined;
    @Field(() => [CommentFilterInput!], { nullable: true })
    AND?: CommentFilterInput[] | undefined;
    @Field(() => [CommentFilterInput!], { nullable: true })
    OR?: CommentFilterInput[] | undefined;
}

@InputType({ description: "Create input for User" })
export class UserCreateInput {
    @Field(() => String!)
    email!: string;
    @Field(() => String!)
    name!: string;
    @Field(() => String, { nullable: true })
    bio?: string;
}

@InputType({ description: "Update input for User" })
export class UserUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => Date)
    createdAt?: Date;
    @Field(() => Date)
    updatedAt?: Date;
    @Field(() => String)
    email?: string;
    @Field(() => String)
    name?: string;
    @Field(() => String, { nullable: true })
    bio?: string;
}

@InputType({ description: "Create input for Post" })
export class PostCreateInput {
    @Field(() => String!)
    title!: string;
    @Field(() => String!)
    content!: string;
    @Field(() => Boolean!)
    published!: boolean;
    @Field(() => Int!)
    viewCount!: number;
    @Field(() => String!)
    authorId!: string;
}

@InputType({ description: "Update input for Post" })
export class PostUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => Date)
    createdAt?: Date;
    @Field(() => Date)
    updatedAt?: Date;
    @Field(() => String)
    title?: string;
    @Field(() => String)
    content?: string;
    @Field(() => Boolean)
    published?: boolean;
    @Field(() => Int)
    viewCount?: number;
    @Field(() => String)
    authorId?: string;
}

@InputType({ description: "Create input for Category" })
export class CategoryCreateInput {
    @Field(() => String!)
    name!: string;
    @Field(() => String, { nullable: true })
    description?: string;
}

@InputType({ description: "Update input for Category" })
export class CategoryUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => String)
    name?: string;
    @Field(() => String, { nullable: true })
    description?: string;
}

@InputType({ description: "Create input for PostCategory" })
export class PostCategoryCreateInput {
    @Field(() => String!)
    postId!: string;
    @Field(() => String!)
    categoryId!: string;
    @Field(() => Date!)
    assignedAt!: Date;
}

@InputType({ description: "Update input for PostCategory" })
export class PostCategoryUpdateInput {
    @Field(() => String)
    postId?: string;
    @Field(() => String)
    categoryId?: string;
    @Field(() => Date)
    assignedAt?: Date;
}

@InputType({ description: "Create input for Comment" })
export class CommentCreateInput {
    @Field(() => String!)
    content!: string;
    @Field(() => String!)
    postId!: string;
    @Field(() => String!)
    authorId!: string;
}

@InputType({ description: "Update input for Comment" })
export class CommentUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => Date)
    createdAt?: Date;
    @Field(() => String)
    content?: string;
    @Field(() => String)
    postId?: string;
    @Field(() => String)
    authorId?: string;
}

@InputType()
export class UserQueryArgs {
    @Field(() => UserFilterInput, { nullable: true })
    filter?: UserFilterInput | undefined;
    @Field(() => UserSortInput, { nullable: true })
    sort?: UserSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class PostQueryArgs {
    @Field(() => PostFilterInput, { nullable: true })
    filter?: PostFilterInput | undefined;
    @Field(() => PostSortInput, { nullable: true })
    sort?: PostSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class CategoryQueryArgs {
    @Field(() => CategoryFilterInput, { nullable: true })
    filter?: CategoryFilterInput | undefined;
    @Field(() => CategorySortInput, { nullable: true })
    sort?: CategorySortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class PostCategoryQueryArgs {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class CommentQueryArgs {
    @Field(() => CommentFilterInput, { nullable: true })
    filter?: CommentFilterInput | undefined;
    @Field(() => CommentSortInput, { nullable: true })
    sort?: CommentSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}
